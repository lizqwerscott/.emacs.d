(require 'separedit)
(keymap-set prog-mode-map "C-c '" #'separedit)
(keymap-set minibuffer-local-map "C-c '" #'separedit)
(keymap-set help-mode-map "C-c '" #'separedit)
(with-eval-after-load 'helpful
  (keymap-set helpful-mode-map "C-c '" #'separedit))

(setq separedit-default-mode 'org-mode)
(setq separedit-remove-trailing-spaces-in-comment t)
(setq separedit-continue-fill-column t)
(setq separedit-buffer-creation-hook #'auto-fill-mode)

(provide 'init-separedit)
