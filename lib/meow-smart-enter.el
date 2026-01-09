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
(require 'org)

(require 'meow-util)


(defun meow-smart-enter-org--at-link-p ()
  "Return non-nil if point is on an Org link."
  (and (or (org-in-regexp org-link-bracket-re 1)
           (org-in-regexp org-link-plain-re 1)
           (org-in-regexp org-link-any-re 1))
       (not (looking-back "\\]\\]" (- (point) 2)))))

(defun meow-smart-enter-org ()
  "Smart RET for Org-mode.
If point is on a link, open it. Otherwise, do `org-return`."
  (interactive)
  (if (meow-smart-enter-org--at-link-p)
      (condition-case nil
          (org-open-at-point)
        (user-error (org-return)))
    (org-return)))

(defun meow-smart-enter-goto-address ()
  "Goto address at point when meow normal mode."
  (interactive)
  (when (meow-normal-mode-p)
    (call-interactively #'goto-address-at-point)))

(defun meow-smart-enter ()
  "Meow smart enter in normal state."
  (interactive)
  (if (eq major-mode 'org-mode)
      (meow-smart-enter-org)
    (call-interactively #'newline-and-indent)))

(define-key goto-address-highlight-keymap (kbd "RET") #'meow-smart-enter-goto-address)

(with-eval-after-load 'meow
  (define-key meow-normal-state-keymap (kbd "RET") #'meow-smart-enter))

(provide 'meow-smart-enter)
;;; meow-smart-enter.el ends here
