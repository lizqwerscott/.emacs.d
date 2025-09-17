;;; indent-yank.el --- indent yank                   -*- lexical-binding: t; -*-

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

;;  source from https://github.com/bohonghuang/indent-yank and  https://emacs-china.org/t/indent-yank/19524

;;; Code:

(require 'dash)

(define-minor-mode indent-yank-mode
  "Minor mode for yanking based on indentation at point.
When this minor mode is enabled, yanked text will automatically be indented
to match the current indentation level at point."
  :lighter " IYank"
  :group 'indent-yank)

(defun indent-yank-line-empty-p ()
  "Check if current line is empty.
Returns non-nil if the current line contains only whitespace characters
and possibly `fill-prefix' characters."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "\\_>*$")))

(defun indent-yank-remove-indent-in-string (str)
  "Remove common indentation from string STR.
This function calculates the minimum indentation level across all non-empty
lines in the string and removes that amount of leading whitespace from each
line."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((indent most-positive-fixnum))
      (while (not (eobp))
        (when (not (indent-yank-line-empty-p))
          (setq indent (min indent (current-indentation))))
        (forward-line 1))
      (goto-char (point-min))
      (while (not (eobp))
        (when (>= (current-indentation) indent)
          (beginning-of-line)
          (delete-char indent))
        (forward-line 1)))
    (buffer-string)))

(defun indent-yank-yank (&optional arg)
  "Yank text with proper indentation.
When called interactively with prefix argument ARG, yank the ARGth killed text.
This function adjusts the indentation of the yanked text to match the current
indentation level at point."
  (interactive "*P")
  (let* ((arg (or arg 1))
         (indent (current-indentation))
         (text-without-indent (indent-yank-remove-indent-in-string (current-kill 0)))
         (text-yank (replace-regexp-in-string
                     "\n" (concat "\n" (-repeat indent ? )) text-without-indent)))
    (let ((head (nth (- arg 1) kill-ring)))
      (setf (nth (- arg 1) kill-ring) text-yank)
      (yank arg)
      (setf (car kill-ring) head))))

(defun indent-yank-before-insert-for-yank (args)
  "Filter function to adjust indentation before yanking.
When `indent-yank-mode' is active, this function processes the text being
yanked to ensure proper indentation based on the current context.
ARGS is the list of arguments passed to `insert-for-yank'."
  (if indent-yank-mode (list (replace-regexp-in-string "\n" (concat "\n" (-repeat (current-indentation) ? )) (indent-yank-remove-indent-in-string (car args))))
    args))

(advice-add #'insert-for-yank :filter-args #'indent-yank-before-insert-for-yank)

(provide 'indent-yank)
;;; indent-yank.el ends here
