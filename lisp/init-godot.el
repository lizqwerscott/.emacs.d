;;; init-godot.el --- init godot                     -*- lexical-binding: t; -*-

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

(with-eval-after-load 'gdscript-mode
  (setf gdscript-use-tab-indents nil))

;;; for gdscript
(define-advice jsonrpc--process-filter (:filter-args (args) fix-newline)
  "gdscript eglot."
  (when (string-match-p "\\` \\*EGLOT (.+/.*gdscript-.+) output\\*\\'"
                        (buffer-name (process-buffer (car args))))
    (setcdr args (list (string-replace "\n\n" "\r\n\r\n" (cadr args)))))
  args)

(provide 'init-godot)
;;; init-godot.el ends here
