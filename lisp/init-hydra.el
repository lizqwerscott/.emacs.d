;;; init-hydra.el --- init hydra                     -*- lexical-binding: t; -*-

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

(require 'lib-hydra)
(pretty-hydra-define-e hydra-toggles
  (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on") :color amaranth :quit-key ("C-g" "q" "<escape>"))
  ("Basic"
   (("w" toggle-sub-word-or-super-word "sub or super word" :toggle (bound-and-true-p subword-mode))
    ("e" electric-pair-mode "electric pair" :toggle t)
    ("s" super-save-mode "auto save" :toggle t)
    ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
    ("c" global-centered-cursor-mode "centered cursor" :toggle t)
    ("i" immersive-translate-auto-mode "immersive translate" :toggle t)
    ("t" +lizqwer/toggle-telega "telega" :toggle (get-buffer "*Telega Root*"))
    ("p" global-jinx-mode "Spell check" :toggle (bound-and-true-p jinx-mode)))
   "Highlight"
   (("h l" global-hl-line-mode "line" :toggle t)
    ("h p" show-paren-mode "paren" :toggle t)
    ("h s" symbol-overlay-mode "symbol" :toggle t)
    ("h r" colorful-mode "colorful" :toggle t)
    ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace)) "whitespace" :toggle show-trailing-whitespace)
    ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
    ("h i" indent-bars-mode "indent" :toggle t))
   "Ui"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("d" +lizqwer/toggle-dark-theme "dark theme" :toggle (cl-find user/night-theme custom-enabled-themes))
    ("T" +lizqwer/toggle-transparent "transparent" :toggle (not (eq (frame-parameter (selected-frame) 'alpha-background) 100)))
    ("r" redacted-mode "redacted" :toggle t)
    ("b" imenu-list-smart-toggle "imenu list" :toggle imenu-list-minor-mode)
    ("k" keycast-log-mode "keycast" :toggle t)
    ("o" outli-mode "Outline" :toggle t)
    ("m t" display-time-mode "Modeline time" :toggle t)
    ("m b" display-battery-mode "Modeline battery" :toggle t))
   "Program"
   (("v" global-diff-hl-mode "gutter" :toggle t)
    ("M" diff-hl-margin-mode "margin gutter" :toggle t)
    ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
   "Customize"
   (("S" customize-save-variable "Customize save variable" :exit t)
    ("C" customize-set-variable "Customize set variable" :exit t)
    ("G" customize-group "Customize set group" :exit t))))

(global-bind-keys
 ("C-c T" . hydra-toggles/body)
 ("<f6>" . hydra-toggles/body))

(provide 'init-hydra)
;;; init-hydra.el ends here
