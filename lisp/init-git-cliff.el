;;; init-git-cliff.el --- init git cliff package     -*- lexical-binding: t; -*-

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

;; Directly
(require 'git-cliff)

;; OPTIONAL
;; Integrate to `magit-tag'
(with-eval-after-load 'magit-tag
  (transient-append-suffix 'magit-tag
    '(1 0 -1)
    '("c" "changelog" git-cliff-menu)))

(provide 'init-git-cliff)
;;; init-git-cliff.el ends here
