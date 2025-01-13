;;; init-codeium.el --- init codeium package         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

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

(setq codeium-command-executable
      (expand-file-name
       (pcase system-type
         ('windows-nt "codeium_language_server.exe")
         (_ "codeium_language_server"))
       (expand-file-name "var/codeium" user-emacs-directory)))
(require 'codeium)
;; (add-hook 'prog-mode-hook
;;           #'(lambda ()
;;               (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)))

(require 'codeium-overlay)

(add-hook 'prog-mode-hook
          #'codeium-overlay-mode)

(provide 'init-codeium)
;;; init-codeium.el ends here
