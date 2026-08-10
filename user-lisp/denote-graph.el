;;; denote-graph.el --- ForceAtlas2 graph of the Denote network -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: convenience, tools, files

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (denote))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library renders the Denote file network as an interactive
;; ForceAtlas2 graph, using the `graph-fa2' package as the layout and
;; rendering engine.
;;
;; Node size encodes connectivity (degree): nodes linked more often are
;; drawn bigger.  The mapping from degree to radius is controlled by the
;; variables `denote-graph-fa2-radius-min' and
;; `denote-graph-fa2-radius-max', and rendering colours by
;; `denote-graph-fa2-node-colour' and `denote-graph-fa2-focus-colour'.
;;
;; Commands:
;;
;; - `denote-graph-fa2-network' : display the whole Denote network.
;; - `denote-graph-current-network' : display a subgraph around the
;;   current file or the Denote link at point.
;; - `denote-graph-fa2-search-node' : complete over the nodes of the
;;   graph in the current buffer and open the selected Denote file.
;;
;; Clicking any node opens the corresponding Denote file (see
;; `graph-fa2').  Graph buffers use the major mode `denote-graph-mode',
;; which binds `n' to `denote-graph-current-network' (to rebuild the
;; current subgraph from its source note) and `s' to
;; `denote-graph-fa2-search-node'.

;;; Code:

(require 'graph-fa2)
(require 'denote)

(defgroup denote-graph nil
  "ForceAtlas2 graph visualisation of the Denote network."
  :group 'denote)

(defcustom denote-graph-fa2-radius-min 8.0
  "Minimum node radius, in SVG units, used by `graph-fa2'."
  :type 'float
  :group 'denote-graph)

(defcustom denote-graph-fa2-radius-max 20.0
  "Maximum node radius, in SVG units, used by `graph-fa2'."
  :type 'float
  :group 'denote-graph)

(defcustom denote-graph-fa2-node-colour "#89b4fa"
  "Fill colour of the regular nodes in a `graph-fa2' layout."
  :type 'color
  :group 'denote-graph)

(defcustom denote-graph-fa2-focus-colour "#f9e2af"
  "Fill colour of the focal node in a `graph-fa2' layout.

Used by `denote-graph-current-network' to highlight the node around
which the subgraph is computed."
  :type 'color
  :group 'denote-graph)

(defun denote-graph-fa2-open-note (id)
  "Open the Denote file corresponding to ID.

ID is a Denote file identifier.  This function is also used as the
node-clicked handler of `graph-fa2'."
  (when-let* ((file (car (denote-directory-files id))))
    (find-file file)))

(defun denote-graph-fa2--collect-links (files)
  "Return links among FILES as a list of \\='(SOURCE-ID . TARGET-ID)\\='.

Scan each file in FILES for references to Denote identifiers (see
`denote-date-identifier-regexp') and record one link per reference, from the
source file identifier to the referenced identifier."
  (let ((edges nil))
    (let ((links-xref (xref-matches-in-files (concat "denote:" denote-date-identifier-regexp) files)))
      (dolist (match links-xref)
        (let* ((loc (xref-match-item-location match))
               (source-file (xref-location-group loc))
               (source-id (denote-retrieve-filename-identifier source-file))
               (summary (xref-match-item-summary match)))
          (when (string-match denote-date-identifier-regexp summary)
            (let ((target-id (match-string 0 summary)))
              (push (cons source-id target-id) edges))))))
    edges))

(defun denote-graph-fa2--degrees (edges)
  "Return a hash table mapping each node in EDGES to its degree.

The degree counts how many times a node appears as either endpoint of
an edge in EDGES, a list of \\='(SRC . TGT)\\=' cons pairs."
  (let ((degree (make-hash-table :test #'equal)))
    (dolist (e edges)
      (let ((src (car e))
            (tgt (cdr e)))
        (puthash src (1+ (gethash src degree 0)) degree)
        (puthash tgt (1+ (gethash tgt degree 0)) degree)))
    degree))

(defun denote-graph-fa2--max-degree (ids degree)
  "Return the maximum degree among IDS, looked up in hash table DEGREE.

Return at least 1 to avoid a division by zero when scaling radii."
  (max 1
       (apply #'max 0
              (mapcar (lambda (id) (gethash id degree 0)) ids))))

(defun denote-graph-fa2--radius-from-degree (degree max-degree)
  "Return the node radius for DEGREE out of MAX-DEGREE.

DEGREE is the degree of a node, MAX-DEGREE the maximum degree in the
graph.  The degree is mapped linearly into the interval bounded by
`denote-graph-fa2-radius-min' and `denote-graph-fa2-radius-max': a
node with MAX-DEGREE gets the maximum radius and a node with zero
degree gets the minimum."
  (+ denote-graph-fa2-radius-min
     (* (- denote-graph-fa2-radius-max denote-graph-fa2-radius-min)
        (if (> max-degree 0)
            (/ (float degree) max-degree)
          0.0))))

(defun denote-graph-fa2--nodes-in-buffer (buffer)
  "Return nodes of the graph displayed by BUFFER as \\='(ID . LABEL)\\='.

Read the simulation context from BUFFER, resolving any indirect
parent buffer, and return an alist of cons pairs whose car is the node
id and whose cdr is the node label.  Return nil when BUFFER has no
live graph."
  (when-let* ((ctx (graph-fa2--discover-context (or (buffer-base-buffer buffer)
                                                    buffer))))
    (mapcar (lambda (n)
              (cons (graph-fa2-id n) (graph-fa2-label n)))
            (graph-fa2-ctx-nodes ctx))))

(defun denote-graph-fa2--neighbors (file all-files)
  "Return Denote files adjacent to FILE.

Combines outgoing links (FILE links to them) and backlinks (they link
to FILE).  ALL-FILES is a list of Denote files to search for linked
targets; it is passed to `denote-get-links' to avoid re-scanning the
directories.  Each file is returned at most once, deduplicated by its
identifier."
  (let ((result nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (nf (append (denote-get-links file all-files)
                        (denote-get-backlinks file)))
      (let ((nid (denote-retrieve-filename-identifier nf)))
        (when (and nid (not (gethash nid seen)))
          (puthash nid t seen)
          (push nf result))))
    result))

(defvar-local denote-graph-fa2-source-file nil
  "Denote file the current graph buffer was generated from.
Used by `denote-graph-current-network' to rebuild a subgraph from a
graph buffer, which itself has no file associated with it.")

(defvar denote-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'denote-graph-current-network)
    (define-key map (kbd "s") #'denote-graph-fa2-search-node)
    map)
  "Keymap for `denote-graph-mode'.")

(define-derived-mode denote-graph-mode special-mode "Denote-Graph"
  "Major mode for ForceAtlas2 graphs of the Denote network.

Graph buffers created by `denote-graph-fa2-network' and
`denote-graph-current-network' use this major mode.  It inherits the
behaviour of `special-mode' and adds denote-specific keys:

\\{denote-graph-mode-map}

The minor mode `graph-fa2-mode' supplies mouse interaction and
zooming, and clicking a node opens its Denote file via
`denote-graph-fa2-open-note'."
  (graph-fa2-mode 1)
  (add-hook 'graph-fa2-node-clicked-functions
            #'denote-graph-fa2-open-note nil t))

;;;###autoload
(defun denote-graph-fa2-network ()
  "Generate and display a ForceAtlas2 graph of the whole Denote network.

Node size reflects the number of links a node has (its degree),
scaled between `denote-graph-fa2-radius-min' and
`denote-graph-fa2-radius-max'.  Clicking a node opens its file."
  (interactive)
  (let* ((files (denote-directory-files nil nil t))
         (edges (denote-graph-fa2--collect-links files))
         (degree (denote-graph-fa2--degrees edges))
         (ids (mapcar #'denote-retrieve-filename-identifier files))
         (max-deg (denote-graph-fa2--max-degree ids degree))
         (nodes (mapcar
                 (lambda (file)
                   (let* ((id (denote-retrieve-filename-identifier file))
                          (type (denote-filetype-heuristics file)))
                     (list :id id
                           :label (denote-retrieve-title-or-filename file type)
                           :colour denote-graph-fa2-node-colour
                           :radius (denote-graph-fa2--radius-from-degree
                                    (gethash id degree 0) max-deg))))
                 files))
         (buf (get-buffer-create "*denote-graph-fa2*")))
    (with-current-buffer buf
      (denote-graph-mode))
    (pop-to-buffer buf)
    (graph-fa2-start buf nodes edges)))

;;;###autoload
(defun denote-graph-current-network (arg)
  "Display a ForceAtlas2 graph of Denote nodes connected to focal node.

The focal node is:

- In a Denote file: the Denote link at point, if any, otherwise the
  current file.
- In a graph buffer (`denote-graph-mode'): the source file that the
  graph was generated from (`denote-graph-fa2-source-file').

The focal node is highlighted with `denote-graph-fa2-focus-colour'.
With numeric prefix ARG, include nodes up to ARG hops away (default
1: only directly connected nodes)."
  (interactive "p")
  (let* ((in-graph-p (derived-mode-p 'denote-graph-mode))
         (source-file (if in-graph-p
                          denote-graph-fa2-source-file
                        (and buffer-file-name
                             (denote-file-has-identifier-p buffer-file-name)
                             buffer-file-name))))
    (unless source-file
      (user-error "Not a Denote file and no graph source to regenerate"))
    (let* ((all-files (denote-directory-files nil nil nil nil :has-identifier))
           (file-by-id (make-hash-table :test #'equal))
           (focus-id (if in-graph-p
                         (denote-retrieve-filename-identifier source-file)
                       (or (denote-get-link-identifier-or-query-term-at-point)
                           (denote-retrieve-filename-identifier source-file)))))
      (dolist (file all-files)
        (puthash (denote-retrieve-filename-identifier file) file file-by-id))
      (unless (gethash focus-id file-by-id)
        (user-error "Focus node `%s' not found in denote directory" focus-id))
      (let* ((subgraph (denote-graph-fa2--subgraph focus-id (or arg 1)
                                                    all-files file-by-id))
             (nodes (car subgraph))
             (edges (cdr subgraph))
             (buf (get-buffer-create "*denote-link-graph*")))
        (with-current-buffer buf
          (denote-graph-mode)
          (setq-local denote-graph-fa2-source-file source-file))
        (pop-to-buffer buf)
        (graph-fa2-start buf nodes edges)))))

(defun denote-graph-fa2--subgraph (focus-id depth all-files file-by-id)
  "Return subgraph centred on FOCUS-ID as \='(NODES . EDGES)\='.

Walk up to DEPTH hops from FOCUS-ID, resolving each identifier to a
file through FILE-BY-ID and scanning links via ALL-FILES.  NODES is
the plist list accepted by `graph-fa2-start' and EDGES the edge list;
the focal node is highlighted with `denote-graph-fa2-focus-colour'."
  (let* ((visited (make-hash-table :test #'equal))
         (reachable (list focus-id))
         (edges nil)
         (edge-set (make-hash-table :test #'equal))
         (queue (list (cons focus-id 0))))
    (puthash focus-id t visited)
    (while queue
      (let* ((elem (pop queue))
             (current (car elem))
             (d (cdr elem))
             (current-file (gethash current file-by-id)))
        (dolist (nf (denote-graph-fa2--neighbors current-file all-files))
          (let ((nid (denote-retrieve-filename-identifier nf)))
            (when (and nid (gethash nid visited))
              ;; 记录可达节点之间的边，统一方向避免重复
              (let ((edge (if (string< current nid)
                              (cons current nid)
                            (cons nid current))))
                (unless (gethash edge edge-set)
                  (puthash edge t edge-set)
                  (push edge edges))))
            (when (and nid (< d depth) (not (gethash nid visited)))
              (puthash nid t visited)
              (push nid reachable)
              (setq queue (append queue (list (cons nid (1+ d))))))))))
    (let* ((degree (denote-graph-fa2--degrees edges))
           (max-deg (denote-graph-fa2--max-degree reachable degree))
           (nodes (mapcar
                   (lambda (id)
                     (let* ((file (gethash id file-by-id)))
                       (list :id id
                             :label (denote-retrieve-title-or-filename
                                     file (denote-filetype-heuristics file))
                             :colour (if (string= id focus-id)
                                         denote-graph-fa2-focus-colour
                                       denote-graph-fa2-node-colour)
                             :radius (denote-graph-fa2--radius-from-degree
                                      (gethash id degree 0) max-deg))))
                   reachable)))
      (cons nodes edges))))

;;;###autoload
(defun denote-graph-fa2-search-node (&optional buffer)
  "Open the Denote file of a node in the graph displayed by BUFFER.

BUFFER defaults to the current buffer.  Prompt with completion over
every node currently shown by that graph, displaying each node's
title, then open the file corresponding to the selected node, just as
clicking the node would."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (nodes (denote-graph-fa2--nodes-in-buffer buf))
         (label-to-id (make-hash-table :test #'equal)))
    (unless nodes
      (user-error "No graph nodes found in buffer `%s'" (buffer-name buf)))
    (dolist (node nodes)
      (puthash (cdr node) (car node) label-to-id))
    (let* ((label (completing-read "Open Denote note: "
                                   (mapcar #'cdr nodes) nil t))
           (id (gethash label label-to-id)))
      (denote-graph-fa2-open-note id))))

(provide 'denote-graph)
;;; denote-graph.el ends here
