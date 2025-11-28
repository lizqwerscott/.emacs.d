;;; prompts.el --- manage prompts                    -*- lexical-binding: t; -*-

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

(require 'yaml)

(defvar prompt-templates nil
  "All prompt templates.")

(defun make-prompt-template (path)
  "Create a prompt template from a file at PATH.

The template content is read from the file and all parameter placeholders of the
form {{PARAM}} are extracted.

Return an plist with the following keys:
- =name': The template name (derived from the file name without extension)
- =params': List of parameter names found in the template
- =content': The full template content as a string

If there is a YAML configuration at the beginning of the file, include it in the
return as well.

If PATH cannot be read or is not a valid file, return nil."
  (when-let* ((path (file-truename path)))
    (let ((name (intern (file-name-sans-extension (file-name-nondirectory path))))
          (params)
          (res))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))

        (when (looking-at-p "^---[ \t]*$")
          (forward-line 1)
          (let ((frontmatter-start (point)))

            ;; Search for closing delimiter
            (unless (re-search-forward "^---[ \t]*$" nil t)
              (error "Malformed frontmatter: opening delimiter '---' found but no closing delimiter"))

            ;; Extract frontmatter text (from start to beginning of closing delimiter)
            (let* ((frontmatter-end (match-beginning 0))
                   (frontmatter-str (buffer-substring-no-properties frontmatter-start frontmatter-end)))

              ;; Parse YAML frontmatter
              (let ((parsed-yaml (yaml-parse-string
                                  frontmatter-str
                                  :object-type 'plist
                                  :object-key-type 'keyword
                                  :sequence-type 'list)))
                (let ((tail parsed-yaml))
                  (while tail
                    (let ((key (pop tail))
                          (val (pop tail)))
                      (pcase key
                        ((or :pre :post) (plist-put parsed-yaml key (eval (read val) t)))
                        (:name (setq name (intern val)))
                        (:parents (plist-put parsed-yaml key
                                             (mapcar #'intern (ensure-list (read val)))))))))

                ;; Validate all keys in the parsed YAML
                (let ((current-plist parsed-yaml))
                  (while current-plist
                    (setq current-plist (cddr current-plist))))

                (setq res (append res parsed-yaml))))))

        (while (search-forward-regexp "{{\\([^}]+\\)}}" nil t)
          (let ((param (match-string 1)))
            (unless (member param params)
              (push param params))))

        (setq res (append res
                          (list :system (buffer-substring-no-properties (point) (point-max))
                                :params params))))
      (cons name res))))

(defun get-all-prompts (dir)
  "Return a list of all prompt templates found in directory DIR.
Search for files with extensions .txt, .md, or .org. Each template is created by
=make-prompt-template' and returned as a cons cell (NAME . (CONTENT . PARAMS))."
  (let ((files (directory-files (file-truename dir) t "\\(\\.txt\\|\\.md\\|\\.org\\)$"))
        (res))
    (dolist (file files)
      (when-let* ((template (make-prompt-template file)))
        (push template res)))
    res))

(defun make-prompt (prompt-template &optional params-alist)
  "Generate a prompt from PROMPT-TEMPLATE by parameters from PARAMS-ALIST.
PROMPT-TEMPLATE should be a cons cell (NAME . (CONTENT . PARAMS))
as returned by `make-prompt-template'.
PARAMS-ALIST is an alist where each element is (PARAM-NAME . VALUE).
All occurrences of {{PARAM-NAME}} in the template content are replaced with the
corresponding VALUE. Return the resulting prompt as a string."
  (let* ((content (plist-get prompt-template :system))
         (params (plist-get prompt-template :params)))
    (if params-alist
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (dolist (param params)
            (let ((value (or (alist-get param params-alist nil nil #'equal)
                             ""))
                  (placeholder (format "{{%s}}" param)))
              (goto-char (point-min))
              (while (search-forward placeholder nil t)
                (replace-match value))))
          (buffer-string))
      content)))

(provide 'prompts)
;;; prompts.el ends here
