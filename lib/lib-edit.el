;;; lib-edit.el --- edit functions                   -*- lexical-binding: t; -*-

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

(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  "Insert numbered lines in rectangle selection.
When called interactively, prompt for START-AT, END-AT, STEP, and FORMAT.
START-AT is the number to start counting from.
END-AT is the number to end counting at.
STEP is the increment between numbers.
FORMAT is the format string for each number (e.g., \"%d \" or \"%02d.\")."
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer.
Move point to the position that is PERCENT percent through the buffer.
For example, (goto-percent 50) moves to the middle of the buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  "Scroll up one third of the window height."
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  "Scroll down one third of the window height."
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun scroll-other-window-up-1/3 ()
  "Scroll other window up one third of the window height."
  (interactive)
  (scroll-other-window (/ (window-body-height) 3)))

(defun scroll-other-window-down-1/3 ()
  "Scroll other window down one third of the window height."
  (interactive)
  (scroll-other-window-down (/ (window-body-height) 3)))

;;;###autoload
(defun toggle-sub-word-or-super-word ()
  "Toggle between subword mode and superword mode."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(provide 'lib-edit)
;;; lib-edit.el ends here
