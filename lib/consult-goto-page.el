;;; consult-goto-page.el --- consult type page       -*- lexical-binding: t; -*-

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

;; from https://emacs-china.org/t/consult-goto-page-consult-el/30998

;;; Code:

(require 'consult)

(defun miruku/collect-pages ()
  "Return a list of page regions in the current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (save-match-data
        (let ((last-point (point-min))
              result)
          (goto-char (point-min))
          (while (re-search-forward page-delimiter nil t)
            (when (= (match-beginning 0) (match-end 0))
              (forward-char))
            (push (cons last-point (point)) result)
            (setf last-point (point)))
          (unless (= last-point (point-max))
            (push (cons last-point (point-max)) result))
          (nreverse result))))))

(defun miruku/goto-page-position (pages str msg)
  "Parse a page specification STR and return a position within PAGES.

Return nil if STR is invalid. The function MSG is called with an error string if
STR is non-empty but invalid.

PAGES is a list of cons cells (START . END) representing page regions.
STR should match the pattern \"PAGE[:LINE[:COL]]\" where PAGE, LINE, and COL are
numbers.
PAGE is 1-indexed. If LINE is omitted or zero, the start of the page is used. If
COL is omitted or zero, the start of the line is used.

The return value is a cons cell (POSITION . (START . END)), where POSITION is
the calculated buffer position, and (START . END) is the region of the selected
page."
  (save-match-data
    (if (and str (string-match "\\`\\([[:digit:]]+\\):?\\([[:digit:]]*\\):?\\([[:digit:]]*\\)\\'" str))
        (let ((page (string-to-number (match-string 1 str)))
              (line (string-to-number (match-string 2 str)))
              (col  (string-to-number (match-string 3 str))))
          (when-let* ((region (nth (1- page) pages)))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (goto-char (car region))
                (when (> line 1) (forward-line (1- line)))
                (goto-char (min (+ (point) col) (pos-eol)))
                (cons (min (point) (1- (cdr region))) region)))))
      (when (and str (not (equal str "")))
        (funcall msg "Please enter a number."))
      nil)))

(defun consult-goto-page (&optional page)
  "Goto PAGE."
  (interactive "P")
  (let ((consult-preview-key 'any)
        (pages (consult--slow-operation "Collecting pages"
                 (miruku/collect-pages))))
    (if page
        (goto-char (or (car (nth (1- page) pages)) (point)))
      (while (if-let* ((pos (save-restriction
                              (miruku/goto-page-position
                               pages
                               (consult--prompt
                                :prompt "Go to page: "
                                :history 'goto-page-history
                                :state
                                (let ((preview (consult--jump-preview)))
                                  (lambda (action str)
                                    (let ((pos (miruku/goto-page-position pages str #'ignore)))
                                      (prog1
                                          (funcall preview action (car pos))
                                        (when pos
                                          (narrow-to-region (cadr pos) (cddr pos))))))))
                               #'consult--minibuffer-message))))
                 (consult--jump (car pos))
               t)))))


(provide 'consult-goto-page)
;;; consult-goto-page.el ends here
