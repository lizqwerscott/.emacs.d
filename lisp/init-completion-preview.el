;;; init-completion-preview.el --- init completion preview  -*- lexical-binding: t; -*-

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
(require 'completion-preview)

(setq completion-preview-minimum-symbol-length 1)
(add-list-to-list 'completion-preview-commands
                  '(hungry-delete-backward
                    outshine-self-insert-command))

(when user/completion-preview-mode-use
  ;; Disable when meow beacon mode
  (advice-add #'meow-grab
              :before
              #'(lambda ()
                  (call-interactively #'completion-preview-mode)))

  (add-hook 'prog-mode-hook #'completion-preview-mode)
  (add-hook 'text-mode-hook #'completion-preview-mode))

(provide 'init-completion-preview)
;;; init-completion-preview.el ends here
