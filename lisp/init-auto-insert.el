;;; init-auto-insert.el --- init auto insert mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  lizqwer scott

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

;; Initialize some configuration about the automatic insert mode

;;; Code:

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))


(add-hook 'find-file-hook 'auto-insert)

(define-auto-insert "\\.sh\\'" [ "defaults-bash.sh" autoinsert-yas-expand ])
(define-auto-insert "\\.vue\\'" [ "defaults-vue.vue" autoinsert-yas-expand ])

(auto-insert-mode t)

(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
