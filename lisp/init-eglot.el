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
        ;; :documentHighlightProvider
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

(keymap-binds eglot-mode-map
  ("C-c j r" . eglot-rename)
  ("C-c j R" . eglot-restart)
  ("C-c j a" . eglot-code-actions)
  ("C-c j s" . consult-eglot-symbols)

  ("M-g u" . eglot-find-implementation))

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

;; next eglot highligh from https://gist.github.com/jdtsmith/8b34f1459ca2a67debf943680f4896a4
(defun my/eglot-next-highlight (&optional prev)
  "Move to the next symbol highlighted by eglot.
Relative position within the symbol is preserved.  Moves to the previous
symbol if PREV is non-nil or called with a prefix argument.  Wraps at
beginning and end of the symbol set."
  (interactive "P")
  (let* ((p (point)) (b (current-buffer))
         (overlays (sort (seq-filter
                          (lambda (o)
                            (eq (overlay-buffer o) b))
                          eglot--highlights)
                         :key #'overlay-start))
         (first (car overlays))
         (offset 0) o done last)
    (while (and overlays (not (eq done t)))
      (setq o (car overlays))
      (cond
       ((or (not o) (not (overlayp o))) 'skip)
       ((eq done 'next)
        (goto-char (min (+ (overlay-start o) offset) (point-max)))
        (setq done t))
       ((and (>= p (overlay-start o))   ;current overlay
             (<= p (overlay-end o)))
        (setq offset (- p (overlay-start o)))
        (if prev
            (progn
              (when last
                (goto-char (min (+ (overlay-start last) offset) (point-max))))
              (setq done t))
          (setq done 'next)))
       (t (setq last o)))
      (setq overlays (cdr overlays)))
    (when (not done) (user-error "No relevant eglot symbols found"))
    (when-let* ((_ (if prev (not last) (eq done 'next))) ; we wrapped
                (wrap (if prev (car (last overlays)) first)))
      (goto-char (+ (overlay-start wrap) offset)))))

(defun my/eglot-prev-highlight ()
  "Eglot prev highligh."
  (interactive)
  (my/eglot-next-highlight 'prev))

(keymap-binds eglot-mode-map
  (("M-g n" "M-g .") . my/eglot-next-highlight)
  (("M-g p" "M-g ,") . my/eglot-prev-highlight))

(defvar-keymap eglot-highlight-repeat-map
  :doc "Eglot highligh repeat map."
  :repeat t
  "n" #'my/eglot-next-highlight
  "p" #'my/eglot-prev-highlight)

(provide 'init-eglot)
;;; init-eglot.el ends here
