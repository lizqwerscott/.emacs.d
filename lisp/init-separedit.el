;;; init-separedit.el --- separedit                  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'separedit)

(setq separedit-default-mode 'org-mode)
(setq separedit-remove-trailing-spaces-in-comment t)
(setq separedit-continue-fill-column t)
(setq separedit-buffer-creation-hook #'auto-fill-mode)

(keymap-sets (prog-mode-map minibuffer-local-map help-mode-map)
  '(("C-c '" . separedit)))

(with-eval-after-load 'helpful
  (keymap-set helpful-mode-map "C-c '" #'separedit))

(provide 'init-separedit)
;;; init-separedit.el ends here
