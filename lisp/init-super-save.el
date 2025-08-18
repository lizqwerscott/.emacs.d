;;; init-super-save.el --- init super-save package   -*- lexical-binding: t; -*-

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

(require 'super-save)

(setq super-save-auto-save-when-idle t
      super-save-idle-duration 1

      super-save-remote-files nil

      super-save-exclude '(".gpg")
      super-save-silent t

      super-save-delete-trailing-whitespace t
      super-save-delete-trailing-whitespace 'except-current-line

      super-save-all-buffers t)

(add-list-to-list 'super-save-triggers
                  '(ace-window unpackaged/magit-status unpackaged/magit-project-status run-or-compile))
(add-to-list 'super-save-hook-triggers 'find-file-hook)

(super-save-mode +1)

(provide 'init-super-save)
;;; init-super-save.el ends here
