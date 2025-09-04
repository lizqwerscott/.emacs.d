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
  (let ((files (directory-files (file-truename dir) t "\\(\\.txt\\|\\.md\\|\\.org\\)$"))
        (res))
    (dolist (file files)
      (when-let* ((template (make-prompt-template file)))
        (push template res)))
    res))

(defun make-prompt (prompt-template params-alist)
  (let* ((content (car prompt-template))
         (params (cdr prompt-template)))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (dolist (param params-alist)
        (let ((placeholder (format "{{%s}}" (car param)))
              (value (cdr param)))
          (while (search-forward placeholder nil t)
            (replace-match value))))
      (buffer-string))))

(provide 'prompts)
;;; prompts.el ends here
