;;; lib-transient.el --- transient utils             -*- lexical-binding: t; -*-

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

(require 'transient)

(defun transient-show--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if v "[x]" "[ ]"))

(defun transient-show--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s"
          (propertize prefix 'face 'font-lock-keyword-face)
          label))

(defun transient-show-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (transient-show--prefix-label label (transient-show--variable-to-checkbox v)))

(defclass transient-toggle (transient-suffix)
  ((toggle :initarg :toggle :initform nil)))

(cl-defmethod transient-format-description ((obj transient-toggle))
  "Get `c/transient-toggle' description.

OBJ is `c/transient-toggle' object."
  (when-let* ((toggle (oref obj toggle))
              (desc (oref obj description)))
    (transient-show-checkbox-label (funcall toggle) desc)))

(defun pretty-transient--process-suffix (suffix)
  "Process SUFFIX to convert :toggle t to proper format."
  (if (and (listp suffix) (>= (length suffix) 3))
      (pcase-let* ((`(,key ,description ,command . ,plist) suffix)
                   (toggle-value (plist-get plist :toggle)))
        (if toggle-value
            (let* ((new-plist (copy-sequence plist))
                   (toggle-func (if (eq toggle-value t)
                                    `(lambda () (bound-and-true-p ,command))
                                  toggle-value)))
              (plist-put new-plist :class 'transient-toggle)
              (plist-put new-plist :toggle toggle-func)
              `(,key ,description ,command ,@new-plist))
          suffix))
    suffix))

(defun pretty-transient--process-group (group)
  "Process GROUP to convert toggle syntax."
  (apply #'vector
         (cl-loop for item across group
                  collect (cond
                           ((vectorp item) (pretty-transient--process-group item))
                           ((listp item) (pretty-transient--process-suffix item))
                           (t item)))))

(defmacro pretty-transient-define-prefix (name arglist &rest args)
  "Define a transient prefix command with simplified toggle syntax.

NAME is `c/transient-prefix' name.
ARGLIST are the arguments that command takes.

ARGS is rest args.

This macro converts :toggle t to the proper `c/transient-toggle' class format."
  (declare (indent defun) (debug ( &define name lambda-list [&rest form])))

  (let ((docstring nil)
        (keywords nil)
        (groups nil)
        (body nil)
        (rest-args args))

    (when (stringp (car rest-args))
      (setq docstring (pop rest-args)))

    (while (and rest-args (keywordp (car rest-args)))
      (let ((keyword (pop rest-args))
            (value (pop rest-args)))
        (push keyword keywords)
        (push value keywords)))
    (setq keywords (nreverse keywords))

    (while (and rest-args (vectorp (car rest-args)))
      (push (pretty-transient--process-group (pop rest-args)) groups))
    (setq groups (nreverse groups))

    (setq body rest-args)

    `(transient-define-prefix ,name ,arglist
       ,@(if docstring (list docstring) nil)
       ,@keywords
       ,@groups
       ,@body)))

(provide 'lib-transient)
;;; lib-transient.el ends here
