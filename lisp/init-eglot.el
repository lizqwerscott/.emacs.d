;;; init-eglot.el --- init eglot package             -*- lexical-binding: t; -*-

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

(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-send-changes-idle-time 0.5)

(require 'eglot)

(setq eglot-ignored-server-capabilities
      '(:inlayHintProvider
        :documentHighlightProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentOnTypeFormattingProvider
        :colorProvider
        :foldingRangeProvider
        ;; :hoverProvider
        ))

(setq eglot-stay-out-of
      '(imenu))

;;; keymap
(defun eglot-restart ()
  "Restart eglot."
  (interactive)
  (call-interactively #'eglot-shutdown)
  (call-interactively #'eglot))

(keymap-sets eglot-mode-map
  '(("C-c j r" . eglot-rename)
    ("C-c j R" . eglot-restart)
    ("C-c j a" . eglot-code-actions)
    ("C-c j s" . consult-eglot-symbols)

    ("M-g u" . eglot-find-implementation)))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode 'json-ts-mode)
                (eglot-ensure))))

(add-hooks '(markdown-mode yaml-mode yaml-ts-mode)
           #'eglot-ensure)

(require 'consult-eglot)
(keymap-set eglot-mode-map
            "C-M-."
            #'consult-eglot-symbols)

;; Emacs LSP booster
(when (executable-find "emacs-lsp-booster")
  (eglot-booster-mode 1))

;; Flycheck
(require 'flycheck-eglot)
(setq-default flycheck-eglot-exclusive nil)
(global-flycheck-eglot-mode 1)

(provide 'init-eglot)
;;; init-eglot.el ends here
