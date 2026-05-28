;;; meow-smart-enter.el --- meow smart enter         -*- lexical-binding: t; -*-

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

(require 'goto-addr)
(require 'meow-util)

(defun meow-smart-enter-goto-address ()
  "Goto address at point when meow normal mode."
  (interactive)
  (cond ((eq major-mode 'org-mode)
         (call-interactively #'org-return))
        (t (call-interactively #'goto-address-at-point))))

(defun meow-smart-enter-keymap-normal-filter (cmd)
  "Return CMD if `browse-url' and similar button bindings should be active.
They are considered active only in read-only buffers."
  (when (meow-normal-mode-p) cmd))

(defun meow-smart-enter-keymap-normal-bind (binding)
  "Behave like BINDING, but only when the buffer is read-only.
BINDING should be a command to pput in a keymap.
Return an element that can be added in a keymap with `keymap-set', such that
it is active only when the current buffer is read-only."
  `(menu-item
    "" ,binding
    :filter ,#'meow-smart-enter-keymap-normal-filter))

(keymap-set goto-address-highlight-keymap "RET" (meow-smart-enter-keymap-normal-bind #'meow-smart-enter-goto-address))

(with-eval-after-load 'org
  (defun meow-smart-enter-org (orig-fn &rest args)
    ""
    (let ((org-return-follows-link (meow-normal-mode-p)))
      (apply orig-fn args)))

  (advice-add #'org-return :around #'meow-smart-enter-org))

(provide 'meow-smart-enter)
;;; meow-smart-enter.el ends here
