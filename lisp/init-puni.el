;;; init-puni.el --- init puni package               -*- lexical-binding: t; -*-

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

(require 'puni)

(defun puni-wrap-double-quote (&optional n)
  "Wrap the following S-expression with double quote brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "\"" "\""))

(defun puni-wrap-round-b ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-round)
    (save-excursion
      (call-interactively #'meow-block)
      (puni-wrap-round))))

(defun puni-wrap-round-r ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-round)
    (save-excursion
      (call-interactively #'meow-mark-word)
      (puni-wrap-round))))

(defun puni-wrap-square-r ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-square)
    (save-excursion
      (call-interactively #'meow-mark-word)
      (puni-wrap-square))))

(defun puni-wrap-curly-r ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-curly)
    (save-excursion
      (call-interactively #'meow-mark-word)
      (puni-wrap-curly))))

(defun puni-wrap-angle-r ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-angle)
    (save-excursion
      (call-interactively #'meow-mark-word)
      (puni-wrap-angle))))

(defun puni-wrap-double-quote-r ()
  (interactive)
  (if (use-region-p)
      (puni-wrap-double-quote)
    (save-excursion
      (call-interactively #'meow-mark-word)
      (puni-wrap-double-quote))))

(defun puni-unwrap ()
  (interactive)
  (puni-squeeze)
  (yank))

;;; from https://github.com/AmaiKinono/puni/issues/47#issuecomment-2298215574
(defun puni-wrap (char)
  "Wrap the region with the given CHAR."
  (cond ((or (eq char ?\)) (eq char ?\()) (puni-wrap-round))
	    ((or (eq char ?\]) (eq char ?\[)) (puni-wrap-square))
	    ((or (eq char ?\}) (eq char ?\{)) (puni-wrap-curly))
	    (t (save-excursion
	         (goto-char (region-end))
	         (insert char)
	         (goto-char (region-beginning))
	         (insert char)))))

(defun meow-wrap-with-pair ()
  "Prompt user to enter a character to wrap the active region, and use `puni-wrap`."
  (interactive)
  (if (use-region-p)
	  (let ((char (read-char "Enter the character to wrap with: ")))
        (puni-wrap char))
    (message "No active region")))

(keymap-sets puni-mode-map
             '(("M-(" . puni-wrap-round-b)
               ("M-[" . puni-wrap-square-r)
               ("M-{" . puni-wrap-curly-r)
               ("M-<" . puni-wrap-angle-r)
               ("M-\"" . puni-wrap-double-quote-r)
               ("M-)" . puni-unwrap)
               ("s-(" . puni-wrap-round-b)
               ("s-[" . puni-wrap-square-r)
               ("s-{" . puni-wrap-curly-r)
               ("s-<" . puni-wrap-angle-r)
               ("s-\"" . puni-wrap-double-quote-r)
               ("s-)" . puni-unwrap)))

(puni-global-mode)

(provide 'init-puni)
;;; init-puni.el ends here
