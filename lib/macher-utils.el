;;; macher-utils.el --- some macher utils function   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

(require 'macher)

(declare-function imenu--flatten-index-alist "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun get-file-outline (path)
  "Return a string representation of the outline for PATH.

The outline is generated from the imenu index and includes the name and
position of each item."
  (when-let* ((path (file-truename path))
              (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (string-join
       (cl-loop for (current . rest) on (cdr (imenu--flatten-index-alist (imenu--make-index-alist) t))
                for next = (car rest)
                collect (concat (substring-no-properties (car current))
                                " [ start pos: "
                                (number-to-string (marker-position (cdr current)))
                                " end pos: "
                                (number-to-string
                                 (if next
                                     (marker-position (cdr next))
                                   (point-max)) )
                                " ]"))
       "\n"))))

(provide 'macher-utils)
;;; macher-utils.el ends here
