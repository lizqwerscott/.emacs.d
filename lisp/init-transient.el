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
(with-eval-after-load 'transient
  (keymap-binds transient-map
    ("<escape>" . transient-quit-all)))

(with-eval-after-load 'calc
  (keymap-binds calc-mode-map
    ("C-o" . casual-calc-tmenu)))

(with-eval-after-load 'info
  (keymap-binds Info-mode-map
    ("C-o" . casual-info-tmenu)))

(with-eval-after-load 'bookmark
  (keymap-binds bookmark-bmenu-mode-map
    ("C-o" . casual-bookmarks-tmenu)))

(with-eval-after-load 'ibuffer
  (keymap-binds ibuffer-mode-map
    ("C-o" . casual-ibuffer-tmenu)
    ("F" . casual-ibuffer-filter-tmenu)
    ("s" . casual-ibuffer-sortby-tmenu)))

(with-eval-after-load 're-builder
  (keymap-binds reb-mode-map
    ("C-o" . casual-re-builder-tmenu))
  (keymap-binds reb-lisp-mode-map
    ("C-o" . casual-re-builder-tmenu)))

(with-eval-after-load 'org-agenda
  (keymap-binds org-agenda-mode-map
    ("C-o" . casual-agenda-tmenu)))

(require 'lib-transient)
(pretty-transient-define-prefix transient-toggles ()
  "Toggles menu."
  :transient-non-suffix 'transient--do-stay
  [["Basic"
    ("w" "Sub or super word" toggle-sub-word-or-super-word
     :toggle (bound-and-true-p subword-mode) :transient t)
    ("e" "Electric pair" electric-pair-mode :toggle t :transient t)
    ("s" "Auto save" super-save-mode :toggle t :transient t)
    ("a" "Aggressive indent" global-aggressive-indent-mode :toggle t :transient t)
    ("c" "Centered cursor" global-centered-cursor-mode :toggle t :transient t)
    ("i" "Immersive translate" immersive-translate-auto-mode :toggle t :transient t)
    ("t" "Telega" +lizqwer/toggle-telega :toggle (get-buffer "*Telega Root*") :transient t)
    ("p" "Spell check" global-jinx-mode :toggle (bound-and-true-p jinx-mode) :transient t)]

   ["Highlight"
    ("h l" "Line highlight" global-hl-line-mode :toggle t :transient t)
    ("h p" "Paren highlight" show-paren-mode :toggle t :transient t)
    ("h s" "Symbol overlay" symbol-overlay-mode :toggle t :transient t)
    ("h r" "colorful" colorful-mode :toggle t :transient t)
    ("h w" "Whitespace"
     (setq-default show-trailing-whitespace
                   (not show-trailing-whitespace))
     :toggle show-trailing-whitespace :transient t)
    ("h d" "Rainbow delimiters" rainbow-delimiters-mode :toggle t :transient t)
    ("h i" "Indent bars" indent-bars-mode :toggle t :transient t)]

   ["Ui"
    ("n" "Line number" display-line-numbers-mode :toggle t :transient t)
    ("d" "Dark theme" +lizqwer/toggle-dark-theme
     :toggle (cl-find user/night-theme custom-enabled-themes) :transient t)
    ("T" "Transparent" +lizqwer/toggle-transparent
     :toggle (not (eq (frame-parameter (selected-frame) 'alpha-background) 100))
     :transient t)
    ("r" "Redacted mode" redacted-mode :toggle t :transient t)
    ("b" "Imenu list" imenu-list-smart-toggle :toggle (bound-and-true-p imenu-list-minor-mode) :transient t)
    ("k" "Keycast log" keycast-log-mode :toggle t :transient t)
    ("o" "Outline" outli-mode :toggle t :transient t)
    ("m t" "Modeline time" display-time-mode :toggle t :transient t)
    ("m b" "Modeline battery" display-battery-mode :toggle t :transient t)]

   ["Program"
    ("v" "Diff-hl gutter" global-diff-hl-mode :toggle t :transient t)
    ("M" "Margin gutter" diff-hl-margin-mode :toggle t :transient t)
    ("E" "Debug on error" toggle-debug-on-error
     :toggle (default-value 'debug-on-error) :transient t)
    ("Q" "Debug on quit" toggle-debug-on-quit
     :toggle (default-value 'debug-on-quit) :transient t)]

   ["Customize"
    ("S" "Customize save variable" customize-save-variable)
    ("C" "Customize set variable" customize-set-variable)
    ("G" "Customize set group" customize-group)]
   ]

  [("q" "Quit" transient-quit-one)])

(provide 'init-transient)
;;; init-transient.el ends here
