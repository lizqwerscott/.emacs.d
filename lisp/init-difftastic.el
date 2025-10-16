;;; init-difftastic.el --- init difftastic package   -*- lexical-binding: t; -*-

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

(require 'difftastic)

;; Add commands to a `magit-difftastic'
(eval-after-load 'magit-diff
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))
(with-hook magit-blame-read-only-mode-hook
  (keymap-binds magit-blame-read-only-mode-map
    ("D" . difftastic-magit-show)
    ("S" . difftastic-magit-show)))

(provide 'init-difftastic)
;;; init-difftastic.el ends here
