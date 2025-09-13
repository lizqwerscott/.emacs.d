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
  (format "%s %s" prefix label))

(defun transient-show-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (transient-show--prefix-label label (transient-show--variable-to-checkbox v)))

(defclass transient-toggle (transient-suffix)
  ((toggle :initarg :toggle :initform nil)))

(cl-defmethod transient-format-description ((obj transient-toggle))
  "Get `c/transient-toggle' description.

OBJ is `c/transient-toggle' object."
  (when-let* ((toggle (oref obj toggle))
              (desc (oref obj description))
              (command (oref obj command)))
    (if (equal toggle 't)
        (transient-show-checkbox-label (if (boundp command)
                                           command
                                         (funcall command))
                                       desc)
      (transient-show-checkbox-label (funcall toggle) desc))))

(provide 'lib-transient)
;;; lib-transient.el ends here
