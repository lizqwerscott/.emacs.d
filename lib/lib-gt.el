;;; lib-gt.el --- gt lib                             -*- lexical-binding: t; -*-

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

(require 'gt)

(defun gt--translate (dict)
  "Translate using DICT from the preset tranlators."
  (gt-start (alist-get dict gt-preset-translators)))

(defun gt-translate-prompt ()
  "Translate with prompt using the multiple dictionaries."
  (interactive)
  (gt--translate 'multi-dict))

(defun gt-use-text-utility ()
  "Handle the texts with the utilities."
  (interactive)
  (gt--translate 'Text-Utility))

;; from https://github.com/agzam/google-translate/blob/translate-popup/google-translate-posframe.el
(defun gt--at-paragraph-boundary-p ()
  "Return non-nil if point is within 5 characters of a paragraph boundary."
  (save-excursion
    (let ((orig-point (point))
          (threshold 5))
      (or
       (progn
         (forward-paragraph -1)
         (skip-chars-forward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))
       (progn
         (goto-char orig-point)
         (forward-paragraph 1)
         (skip-chars-backward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))))))

(defun gt--at-line-beginning-p ()
  "Return t if point is at the beginning of a sentence on the current line."
  (and (looking-at "[A-Z0-9]")
       (looking-back "[.?!]\\s-+" (line-beginning-position))))

(defun gt--taker-paragraph-p ()
  "Return non-nil if the current context suggests paragraph-based text capture."
  (or (cl-find major-mode '(Info-mode help-mode helpful-mode devdocs-mode))
      (gt--at-paragraph-boundary-p)))

(provide 'lib-gt)
;;; lib-gt.el ends here
