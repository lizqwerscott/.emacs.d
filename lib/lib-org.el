;;; lib-org.el --- lib org                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'org)

(defun org-export-docx (&optional template-select)
  "Use pandoc convert org to Docx.

if TEMPLATE-SELECT is not nil, select a template file."
  (interactive "P")
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (file-truename
                        (if template-select
                            (read-file-name "Select Template file: "
                                            (expand-file-name "config/template/"
                                                              user-emacs-directory))
                          (expand-file-name "config/template/template.docx" user-emacs-directory)))))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file)
     (get-buffer-create "*Org export docx*")
     (get-buffer-create "*Org export docx error*"))
    (message "Convert finish: %s" docx-file)))

(defun org-toggle-display-emphasis-markers ()
  "Toggle show emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers
        (not org-hide-emphasis-markers))
  (revert-buffer-quick))

(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

(defun org-insert-item-auto-checkbox ()
  "Org insert auto-checkbox item."
  (interactive)
  (org-insert-item
   (and (org-in-item-p)
        (save-excursion
          (looking-back "\\[[ xX]\\].**" (line-beginning-position))))))

(defun org-meta-return-auto (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item-auto-checkbox' or
`org-table-wrap-region', depending on context.  When called with
an ARG, unconditionally call `org-insert-heading'."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				                ((org-at-table-p) #'org-table-wrap-region)
				                ((org-in-item-p) #'org-insert-item-auto-checkbox)
				                (t #'org-insert-heading)))))

(require 'ox)

(defun org-get-export-file-paths ()
  "Get all possible export file paths for current Org buffer.
Returns an alist where keys are export formats and values are file paths."
  (interactive)
  (let ((paths '())
        (formats '(("html" . ".html")
                   ("latex" . ".tex")
                   ("pdf" . ".pdf")
                   ("markdown" . ".md")
                   ("odt" . ".odt")
                   ("org" . ".org")
                   ("ascii" . ".txt")
                   ("texinfo" . ".texi"))))
    (dolist (format formats)
      (let* ((backend (car format))
             (extension (cdr format))
             (output-file (org-export-output-file-name extension)))
        (when (file-exists-p output-file)
          (push (cons backend output-file) paths))))
    paths))

(defun org-list-export-file ()
  "Open export files Dired buffer."
  (interactive)
  (let* ((export-files (org-get-export-file-paths))
         (files (mapcar #'cdr export-files)))
    (cond
     ((null files)
      (message "No export files found for current buffer"))
     (t
      (dired (cons default-directory files))))))

(defun my/org-open-links-in-dired ()
  "Open all ‘file:’ links in Dired buffer."
  (interactive)
  (let* ((links
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (when (string= (org-element-property :type link) "file")
                (org-element-property :path link)))))
         (abs-paths
          (mapcar (lambda (path)
                    (expand-file-name path default-directory))
                  links)))
    (if abs-paths
        (dired abs-paths)
      (message "No file links found in this buffer."))))

;; From https://xenodium.com/emacs-dwim-do-what-i-mean
(require 'dom)
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(provide 'lib-org)
;;; lib-org.el ends here
