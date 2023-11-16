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

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

;; Configure directory extension.
(require #'vertico-directory)
(keymap-set vertico-map "RET" #'vertico-directory-enter)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
(keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(provide 'init-minibuffer)
