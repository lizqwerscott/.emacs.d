;;; init-transient.el --- init transient menu        -*- lexical-binding: t; -*-

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
(require 'transient)
(keymap-set transient-map
            "<escape>"
            #'transient-quit-all)

(with-eval-after-load 'calc
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))

(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu))

(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))

(with-eval-after-load 'info
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))

(with-eval-after-load 'bookmark
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu))

(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu))

(provide 'init-transient)
;;; init-transient.el ends here
