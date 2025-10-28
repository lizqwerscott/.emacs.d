;;; editkit.el --- edit kit                          -*- lexical-binding: t; -*-

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

(require 'transient)
(require 'grugru)

(defun my-toggle-case-current-word ()
  "Toggle the case of the current word between lower, upper, and capitalized."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word))
          (case-fold-search nil))
      (when bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (word (buffer-substring-no-properties beg end)))
          (cond
           ((string= word (upcase word))
            (downcase-region beg end))
           ((string= word (downcase word))
            (capitalize-region beg end))
           (t
            (UPCASE-REGION beg end))))))))

(defun editkit--dwim-at-point (fn)
  "Dwim at point for FN."
  (interactive)
  (let ((case-fold-search nil))
    (if (use-region-p)
        (funcall fn (region-beginning) (region-end))
      (when-let* ((bounds (bounds-of-thing-at-point 'word))
                  (beg (car bounds))
                  (end (cdr bounds)))
        (funcall fn beg end)))))

(defun editkit-capitalize-dwim-at-point ()
  "Capitalize dwim at point."
  (interactive)
  (editkit--dwim-at-point #'capitalize-region))

(defun editkit-downcase-dwim-at-point ()
  "Downcase dwim at point."
  (interactive)
  (editkit--dwim-at-point #'downcase-region))

(defun editkit-upcase-dwim-at-point ()
  "Upcase dwim at point."
  (interactive)
  (editkit--dwim-at-point #'upcase-region))

(defun editkit-buffer-read-only-p ()
  "Predicate for `buffer-read-only'."
  (if buffer-read-only t nil))

(transient-define-prefix editkit-rectangle-menu ()
  "Editkit Rectangle menu."
  :refresh-suffixes t
  :transient-non-suffix 'transient--do-stay
  ["Rectangle"
   ["mark"
    ("m" "mark" rectangle-mark-mode :transient t)]

   ["edit"
    ("K" "kill" kill-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("c" "Copy" copy-rectangle-as-kill
     :inapt-if-not use-region-p
     :transient t)
    ("y" "Yank" yank-rectangle
     :if-not editkit-buffer-read-only-p
     :transient t)
    ("d" "Delete" delete-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)]

   ["Replace"
    ("s" "String…" string-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("i" "String Insert…" string-insert-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("o" "Open Insert" open-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)]

   ["Misc"
    ("N" "Number" rectangle-number-lines
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("C" "Clear" clear-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("D" "Delete Leading Spaces" delete-whitespace-rectangle
     :if-not editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("q" "Done" transient-quit-all)]])

(transient-define-prefix editkit-transform-menu ()
  "Editkit transform menu."
  :transient-non-suffix 'transient--do-stay
  ["Transform"
   [("c" "Capitalize" editkit-capitalize-dwim-at-point :transient t)]

   [("L" "Make Lower Case" editkit-downcase-dwim-at-point :transient t)
    ("u" "Make Upper Case" editkit-upcase-dwim-at-point :transient t)]

   [("g" "grugru" grugru :transient t)]]
  [("q" "Done" transient-quit-all)])

(provide 'editkit)
;;; editkit.el ends here
