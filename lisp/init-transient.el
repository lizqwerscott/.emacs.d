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

;; (with-eval-after-load 'dired
;;   (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))

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

(transient-define-prefix elpaca-dispath ()
  "Elpaca package manage dispatch"
  [["Manage package"
    ("e" "Pakcage manager" elpaca-manager)
    ("t" "Try package" elpaca-try)
    ("r" "Rebuild package" elpaca-rebuild)
    ("d" "Delete package" elpaca-delete)]
   ["Elpaca Ui"
    ("s s" "Search package" elpaca-ui-search)
    ("s i" "Search installed" elpaca-ui-search-installed)
    ("s o" "Search orphaned" elpaca-ui-search-orphaned)
    ("s t" "Search tried" elpaca-ui-search-tried)]
   ["Update"
    ("f f" "Fetch package" elpaca-fetch)
    ("f a" "Fetch all package" elpaca-fetch-all)
    ("m m" "Merge package" elpaca-merge)
    ("m a" "Merge all package" elpaca-merge-all)
    ("u u" "Update package" elpaca-update)
    ("u a" "Update all package" elpaca-update-all)]
   ["Package Info"
    ("i" "Package Info" elpaca-info)
    ("l" "Pakcage Log" elpaca-log)
    ("b" "Browse package website" elpaca-browse)
    ("v v" "Visit package" elpaca-visit)
    ("v b"
     "Vist package build"
     (lambda ()
       (interactive)
       (let ((current-prefix-arg 1))
         (call-interactively #'elpaca-visit))))]]
  [("q" "Quit" transient-quit-all)])

(with-eval-after-load 'elpaca-manager
  (keymap-sets elpaca-manager-mode-map
    '(("C-o" . elpaca-dispath)
      ("p" . previous-line))))

(provide 'init-transient)
;;; init-transient.el ends here
