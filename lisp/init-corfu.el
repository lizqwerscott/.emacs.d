;;; init-corfu.el --- init corfu package             -*- lexical-binding: t; -*-

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

(defun +complete ()
  (interactive)
  (or ;; (tempel-complete t)
   (yas-expand)
   (completion-preview-insert)
   (corfu-next)))

(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match t
      corfu-auto-prefix 2
      corfu-preview-current nil
      corfu-auto-delay 0.0
      corfu-popupinfo-delay '(0.4 . 0.2))

(custom-set-faces
 '(corfu-border ((t (:inherit region :background unspecified)))))

(keymap-sets corfu-map
             '(("TAB" . +complete)
               ;; ("[tab]" . +complete)
               ("S-TAB" . corfu-previous)
               ;; ("[backtab]" . corfu-previous)
               ))

(pcase user/lsp-client
  ('eglot
   (add-hook 'after-init-hook #'global-corfu-mode))
  ('lsp-bridge
   (add-hooks '(rust-mode sly-mrepl-mode scheme-mode sql-mode eshell-mode inferior-python-mode elvish-mode telega-chat-mode)
              #'corfu-mode)))

(add-hook 'global-corfu-mode-hook #'corfu-popupinfo-mode)

(corfu-history-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)


;;; cpae
(add-list-to-list 'completion-at-point-functions
                  '(cape-dabbrev
                    cape-file
                    cape-elisp-block
                    cape-keyword
                    cape-abbrev))

(require 'cape)

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-abbrev
                     #'cape-file))
              cape-dabbrev-min-length 7))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

(defun my/ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
     (eq (char-after (car completion-in-region--data)) ?:)))

(defun my/setup-elisp ()
  (setq-local completion-at-point-functions
              `(,(cape-capf-super
                  (cape-capf-predicate
                   #'elisp-completion-at-point
                   #'my/ignore-elisp-keywords)
                  #'cape-dabbrev)
                cape-file)
              cape-dabbrev-min-length 5))
(add-hook 'emacs-lisp-mode-hook #'my/setup-elisp)

;;; kind icon
;; (require 'kind-icon)
;; (setq kind-icon-default-face 'corfu-default
;;       kind-icon-use-icons nil)
;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;; (setq completion-cycle-threshold 3)
;; (setq tab-always-indent 'complete)

;;; nerd icons corfu
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(require 'nerd-icons-corfu)

(provide 'init-corfu)
;;; init-corfu.el ends here
