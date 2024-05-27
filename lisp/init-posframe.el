;;; init-posframe.el --- init posframe package       -*- lexical-binding: t; -*-

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

(defface posframe-border
  `((t (:inherit region)))
  "Face used by the `posframe' border."
  :group 'posframe)

(defvar posframe-border-width 2
  "Default posframe border width.")

(defun posframe-poshandler-frame-center-near-bottom (info)
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (/ (+ (plist-get info :parent-frame-height)
              (* 2 (plist-get info :font-height)))
           2)))

(provide 'init-posframe)
;;; init-posframe.el ends here
