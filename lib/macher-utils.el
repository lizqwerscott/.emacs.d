;;; macher-utils.el --- some macher utils function   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

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

;;

;;; Code:

(require 'macher)

(declare-function imenu--flatten-index-alist "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun get-file-outline (path)
  "Return a string representation of the outline for PATH.

The outline is generated from the imenu index and includes the name and
position of each item."
  (when-let* ((path (file-truename path))
              (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (string-join
       (cl-loop for (current . rest) on (cdr (imenu--flatten-index-alist (imenu--make-index-alist) t))
                for next = (car rest)
                collect (concat (substring-no-properties (car current))
                                " [ start pos: "
                                (number-to-string (marker-position (cdr current)))
                                " end pos: "
                                (number-to-string
                                 (if next
                                     (marker-position (cdr next))
                                   (point-max)) )
                                " ]"))
       "\n"))))

(defun macher-context--outline-contents-for-file (path context)
  "Get or create content strings for PATH in the macher CONTEXT.

Returns a cons cell \\=(orig-content new-content).

PATH can be any absolute file path. By default, macher will only call
this for paths within the CONTEXT's workspace.

Returns a cons cell (orig-content . new-content) of strings for the file.
If the orig-content is nil, the file is being created; if the new-content
is nil, the file is being deleted.

Also updates the CONTEXT's :contents alist if the relevant entry was not
yet present. In that case, loads file contents if the file exists;
otherwise returns (nil . nil)."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")
  ;; Normalize the path for consistent lookup.
  (let* ((normalized-path (macher--normalize-path path))
         ;; Check if we already have contents for this file.
         (existing-contents (assoc normalized-path (macher-context-contents context))))
    (if existing-contents
        ;; Return the existing contents.
        (cdr existing-contents)
      ;; Handle file existence check.
      (let* ((file-outline (when (file-exists-p normalized-path)
                             (get-file-outline normalized-path)))
             (context-contents (macher-context-contents context))
             ;; Both original and new content start as the same.
             (content-pair (cons file-outline file-outline)))
        (push (cons normalized-path content-pair) context-contents)
        (setf (macher-context-contents context) context-contents)
        content-pair))))

(defun macher-context--part-contents-for-file (path context start end)
  "Get or create content strings for PATH in the macher CONTEXT.

Returns a cons cell \\=(orig-content new-content).

PATH can be any absolute file path. By default, macher will only call
this for paths within the CONTEXT's workspace.

Returns a cons cell (orig-content . new-content) of strings for the file.
If the orig-content is nil, the file is being created; if the new-content
is nil, the file is being deleted.

Also updates the CONTEXT's :contents alist if the relevant entry was not
yet present. In that case, loads file contents if the file exists;
otherwise returns (nil . nil)."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")
  ;; Normalize the path for consistent lookup.
  (let* ((normalized-path (macher--normalize-path path))
         (buffer (find-file-noselect normalized-path)))
    (with-current-buffer buffer
      (let ((content (buffer-substring start end)))
        (cons content content)))))

(defun my/read-tools (context make-tool-function)
  "Generate read-only tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of read-only tools that allow the LLM to inspect files
in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (let* ((workspace (macher-context-workspace context))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))

         ;; Shared helper to get or create implementation contents for a file.
         (get-or-create-file-outlines
          (lambda (file-path)
            "Get or create implementation contents for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-content . new-content) of strings for the file.
Also updates the context's :contents alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context--outline-contents-for-file full-path context))))
         (get-file-content
          (lambda (file-path start end)
            "Get or create implementation contents for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-content . new-content) of strings for the file.
Also updates the context's :contents alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context--part-contents-for-file full-path context start end)))))
    (list
     (funcall
      make-tool-function
      :name "read_file_outline_in_workspace"
      :function
      `,(lambda (path)
          "Read the contents of a file specified by PATH within the workspace."
          (let* (;; Get implementation contents for this file.
                 (contents (funcall get-or-create-file-outlines path))
                 (new-content (cdr contents)))
            ;; Check if the file exists for reading.
            (if (not new-content)
                (error (format "File '%s' not found in workspace" path))
              ;; Return the content directly.
              new-content)))
      :description
      (concat
       "Read file outlines from the workspace. "
       "The outlines contain the names of functions and classes in the file, as well as their starting and ending positions."
       "Returns the current outlines of a file. Use this ONLY when you need to see a file "
       "that wasn't included in the initial context, or to verify changes after editing. "
       "Do NOT use this to re-read files whose outlines were already provided in the request context.")
      :confirm nil
      :include nil
      :args
      '((:name "path" :type string :description "Path to the file, relative to workspace root")))
     (funcall
      make-tool-function
      :name "read_file_content_in_workspace"
      :function
      `,(lambda (path start end)
          "Read the contents of a file specified by PATH within the workspace."
          (let* (;; Get implementation contents for this file.
                 (contents (funcall get-file-content path start end))
                 (new-content (cdr contents)))
            ;; Check if the file exists for reading.
            (if (not new-content)
                (error (format "File '%s' not found in workspace or start end not find" path))
              ;; Return the content directly.
              new-content)))
      :description
      (concat
       "Read file content from the workspace. "
       "Returns the current content of a file. Use this ONLY when you need to see a file "
       "that wasn't included in the initial context, or to verify changes after editing. "
       "Do NOT use this to re-read files whose content were already provided in the request context.")
      :confirm nil
      :include nil
      :args
      '((:name "path" :type string :description "Path to the file, relative to workspace root")
        (:name "start" :type number :description "The starting position of the file content to be retrieved")
        (:name "end" :type number :description "The end position of the file content to be retrieved"))))))

(provide 'macher-utils)
;;; macher-utils.el ends here
