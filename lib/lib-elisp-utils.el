;;; lib-elisp-utils.el --- elisp utils               -*- lexical-binding: t; -*-

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

(defun mapcar-if (fn seq handle)
  "Apply FN to elements of SEQ that satisfy HANDLE predicate.
FN is a function to apply to each element that satisfies the HANDLE predicate.
SEQ is a sequence of elements to process.
HANDLE is a predicate function that takes an element and returns non-nil if
the element should be processed by FN.
Returns a new list with FN applied to elements that satisfy HANDLE, and
unchanged elements that don't satisfy HANDLE."
  (mapcar #'(lambda (v)
              (if (funcall handle v)
                  (funcall fn v)
                v))
          seq))

(defun mapcar-if-not (fn seq handle)
  "Apply FN to elements of SEQ that do not satisfy HANDLE predicate.
FN is a function to apply to each element that does not satisfy the HANDLE
predicate. SEQ is a sequence of elements to process.
HANDLE is a predicate function that takes an element and returns non-nil if
the element should NOT be processed by FN.
Returns a new list with FN applied to elements that do not satisfy HANDLE, and
unchanged elements that satisfy HANDLE."
  (mapcar #'(lambda (v)
              (if (funcall handle v)
                  v
                (funcall fn v)))
          seq))

(defun add-list-to-list (list-var elements)
  "Add ELEMENTS to LIST-VAR.
ELEMENTS can be a single element or a list of elements.
If ELEMENTS is a list, each element is added to LIST-VAR in reverse order.
This function uses `add-to-list' to add elements, which means duplicates are
not added."
  (if (listp elements)
      (mapcar #'(lambda (element)
                  (add-to-list list-var element))
              (reverse elements))
    (add-to-list list-var elements)))

;;; cons to list
(defun single-cons-p (c)
  "Is C is single cons."
  (and (consp c)
       (not (consp (cdr c)))
       (not (null (cdr c)))))

(defun cons-to-list-s (s)
  "Convert S to list."
  (if (single-cons-p s)
      (list (car s) (cdr s))
    (cons-to-list s)))

(defun cons-to-list (c)
  "Convert C to list."
  (when c
    (if (listp c)
        (cons (cons-to-list-s (car c))
              (cons-to-list-s (cdr c)))
      c)))

(defun match-in (pred lst)
  "Check if any element in LST satisfies the predicate PRED.

Returns t if any element satisfies PRED, nil otherwise."
  (catch 'found
    (dolist (x lst)
      (when (funcall pred x)
        (throw 'found t)))))

(provide 'lib-elisp-utils)
;;; lib-elisp-utils.el ends here
