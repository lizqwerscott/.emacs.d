;;; init-macher.el --- init macher package           -*- lexical-binding: t; -*-

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
(setq macher-action-buffer-ui 'org)

(setf (alist-get 'implement macher-actions-alist)
      (apply-partially #'macher-action-from-region-or-input
                       "To implement: "
                       #'macher--implement-with-cursor-prompt
                       'macher))

(require 'macher-utils)

(macher-install)

(provide 'init-macher)
;;; init-macher.el ends here
