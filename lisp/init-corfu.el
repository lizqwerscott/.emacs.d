
(defun +complete ()
  (interactive)
  (or ;; (tempel-complete t)
   (yas-expand)
   (corfu-next)))

(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match t
      corfu-auto-prefix 2
      corfu-preview-current nil
      corfu-auto-delay 0.2
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
                    cape-file))
(require 'cape)

;;; kind icon
(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default
      kind-icon-use-icons nil)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;; (setq completion-cycle-threshold 3)
;; (setq tab-always-indent 'complete)

(provide 'init-corfu)
