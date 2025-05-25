;;; init-eat.el --- init eat package                 -*- lexical-binding: t; -*-

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

(defun meomacs-eat-meow-setup ()
  (add-hook 'meow-normal-mode-hook 'eat-emacs-mode nil t)
  (add-hook 'meow-insert-mode-hook 'eat-char-mode nil t))

(with-eval-after-load "eat"
  (define-key eat-char-mode-map (kbd "C-y") 'eat-yank)
  ;; Replace semi-char mode with emacs mode
  (advice-add 'eat-semi-char-mode :after 'eat-emacs-mode)
  (add-hook 'eat-mode-hook 'meomacs-eat-meow-setup))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(provide 'init-eat)
;;; init-eat.el ends here
