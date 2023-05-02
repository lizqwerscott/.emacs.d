(require 'vertico)
(require 'marginalia)
(require 'consult)

(vertico-mode 1)

(setq vertico-count 20
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(keymap-set vertico-map "?" #'minibuffer-completion-help)
(keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
(keymap-set vertico-map "M-TAB" #'minibuffer-complete)
(add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
(keymap-set minibuffer-local-map "M-s" #'consullt-history)
(keymap-set minibuffer-local-map "M-r" #'consult-history)

(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

;; (require 'consult-project-extra)

(provide 'init-minibuffer)
