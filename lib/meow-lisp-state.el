;;; meow-lisp-state.el --- meow lisp state           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

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

(require 'puni)
(require 'meow)

(defvar meow-lisp-state-keymap (make-keymap)
  "Meow lisp keymap.")

(meow-define-state lisp
  "meow state for interacting with smartlisps"
  :lighter " [L]"
  :keymap meow-lisp-state-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-lisp 'hollow)

(defun backward-down-list (&optional arg interactive)
  "Move backward down one level of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do this that many times.
A negative argument means move forward but still go down a level.
This command assumes point is not in a string or comment.
Uses `down-list' to do the work.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (or arg (setq arg 1))
  (down-list (- arg) interactive))

(meow-define-keys 'lisp
  '("l" . puni-forward-sexp)
  '("h" . puni-backward-sexp)
  '("j" . down-list)
  '("k" . up-list)
  '("K" . backward-up-list)
  '("J" . backward-down-list)

  '("n" . puni-slurp-forward)
  '("b" . puni-barf-forward)
  '("v" . puni-slurp-backward)
  '("c" . puni-barf-backward)

  '("u" . meow-undo)
  '(";" . exchange-point-and-mark)

  '("d" . meow-kill)

  '("o" . puni-expand-region)
  '("r" . puni-raise)
  '("p" . puni-split)
  '("t" . puni-transpose)
  '("s" . puni-squeeze)

  '("a" . puni-beginning-of-sexp)
  '("e" . puni-end-of-sexp)

  '("i" . meow-insert-mode)
  '("<escape>" . meow-normal-mode)

  '("SPC" . meow-keypad)

  '("(" . puni-wrap-round)
  '("[" . puni-wrap-square)
  '("{" . puni-wrap-curly)
  '("\"" . puni-wrap-double-quote)
  '("'" . puni-wrap-single-quote)
  '("C-j" . puni-jump-out-pair-and-newline)
  '("<" . puni-wrap-angle)
  )

(provide 'meow-lisp-state)
;;; meow-lisp-state.el ends here
