;;; insert-trailing-semi.el --- insert trailing-semi  -*- lexical-binding: t; -*-

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

;; This package provides functions to insert or remove trailing characters
;; (specifically semicolons or commas) at the end of lines. It can operate
;; on the current line or on a selected region.
;;
;; Functions:
;; - `insert-or-remove-trailing-char': Inserts or removes a specified character at end of lines
;; - `insert-or-remove-trailing-semi': Inserts or removes a semicolon at end of lines
;; - `insert-or-remove-trailing-comma': Inserts or removes a comma at end of lines
;; - `insert-trailing-char': Inserts a specified character at end of lines if not already present
;; - `insert-trailing-semi': Inserts a semicolon at end of lines if not already present
;; - `insert-trailing-semi-and-indent': Inserts a semicolon and indents the next line

;;; Code:

(defun insert-or-remove-trailing-char (&optional ch)
  "Insert or remove the trailing character CH at the end of lines.
If CH is not provided, it will prompt for input. If called on a region, it will
process all lines in the region. Otherwise, it processes the current line. If
the line already ends with CH, it removes it; otherwise, it adds it."
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (if (eq (char-before) ch)
                  (delete-char -1)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (forward-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-or-remove-trailing-semi ()
  "Insert or remove a semicolon at the end of lines.
If called on a region, it will process all lines in the region.
Otherwise, it processes the current line.
If the line already ends with a semicolon, it removes it; otherwise, it adds it."
  (interactive)
  (insert-or-remove-trailing-char ?\;))

(defun insert-or-remove-trailing-comma ()
  "Insert or remove a comma at the end of lines.
If called on a region, it will process all lines in the region.
Otherwise, it processes the current line.
If the line already ends with a comma, it removes it; otherwise, it adds it."
  (interactive)
  (insert-or-remove-trailing-char ?,))

(defun insert-trailing-char (&optional ch)
  "Insert the trailing character CH at the end of lines if not already present.
If CH is not provided, it will prompt for input. If called on a region, it will
process all lines in the region. Otherwise, it processes the current line. It
only adds the character if it's not already at the end of the line."
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (unless (eq (char-before) ch)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (forward-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-trailing-semi ()
  "Insert a semicolon at the end of lines if not already present.
If called on a region, it will process all lines in the region.
Otherwise, it processes the current line.
It only adds the semicolon if it's not already at the end of the line."
  (interactive)
  (insert-trailing-char ?\;))

;;;###autoload
(defun insert-trailing-semi-and-indent ()
  "Insert a semicolon at the end of the current line and indent the next line.
It only adds the semicolon if it's not already present.
After inserting the semicolon, it moves to the next line and indents it."
  (interactive)
  (insert-trailing-char ?\;)
  (forward-char)
  (newline-and-indent))

(provide 'insert-trailing-semi)
;;; insert-trailing-semi.el ends here
