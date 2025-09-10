;;; dired-menu.el --- dired menu                     -*- lexical-binding: t; -*-

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
(require 'casual-dired)

(defun transient-show--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if v "[x]" "[ ]"))

(defun transient-show--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun transient-show-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (transient-show--prefix-label label (transient-show--variable-to-checkbox v)))

;;;###autoload
(transient-define-prefix dired-dispatch ()
  "Dired dispatch menu"
  [["Directory"
    ("h" "Hide Details" dired-hide-details-mode
     :description
     (lambda ()
       (transient-show-checkbox-label dired-hide-details-mode "Hide Details")))
    ("o" "Omit Mode" dired-omit-mode
     :description
     (lambda () (transient-show-checkbox-label dired-omit-mode "Omit Mode")))]
   ["Sort By"
    ("n" "Name" casual-dired--sort-by-name :transient t)
    ("k" "Kind" casual-dired--sort-by-kind :transient t)
    ("l" "Date Last Opened" casual-dired--sort-by-date-last-opened
     :transient t)
    ("a" "Date Added" casual-dired--sort-by-date-added :transient t)
    ("m" "Date Modified" casual-dired--sort-by-date-modified :transient t)
    ("M" "Date Metadata Changed" casual-dired--sort-by-date-metadata-changed
     :transient t)
    ("s" "Size" casual-dired--sort-by-size :transient t)]]
  [("q" "Quit" transient-quit-all)])

(provide 'dired-menu)
;;; dired-menu.el ends here
