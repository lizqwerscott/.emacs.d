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

(defvar prompt-templates nil
  "All prompt templates.")

(defun make-prompt-template (path &optional name)
  "Create a prompt template from a file at PATH.
Optional NAME specifies the template name, defaulting to the file's base name
without extension. The template content is read from the file and all parameter
placeholders of the form {{PARAM}} are extracted.
Return a cons cell (NAME . (CONTENT . PARAMS)) where CONTENT is the template
text as a string and PARAMS is a list of parameter names found in the template."
  (when-let* ((path (file-truename path)))
    (let ((name (if name
                    name
                  (intern (file-name-sans-extension (file-name-nondirectory path)))))
          (content)
          (params))
      (with-temp-buffer
        (insert-file-contents path)
        (setq content (buffer-string))
        (goto-char (point-min))
        (while (search-forward-regexp "{{\\([^}]+\\)}}" nil t)
          (let ((param (match-string 1)))
            (unless (member param params)
              (push param params))))
        (setq params (nreverse params)))
      (cons name (cons content params)))))

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
  (let* ((content (car prompt-template))
         (params (cdr prompt-template)))
    (if params-alist
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (dolist (param params)
            (let ((value (or (alist-get param params-alist)
                             ""))
                  (placeholder (format "{{%s}}" param)))
              (while (search-forward placeholder nil t)
                (replace-match value))))
          (buffer-string))
      content)))

(provide 'prompts)
;;; prompts.el ends here
