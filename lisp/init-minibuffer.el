(require 'vertico)
(require 'marginalia)
(require 'consult)

;;; Emacs complection

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

;; Only list the commands of the current modes
(when (boundp 'read-extended-command-predicate)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

;;; Vertico
(setq vertico-count 20)
(keymap-set minibuffer-local-map "M-s" #'consult-history)
(keymap-set minibuffer-local-map "M-r" #'consult-history)
(keymap-set minibuffer-local-map "C-i" #'(lambda ()
                                           "Insert the currunt symbol."
                                           (interactive)
                                           (insert (save-excursion
		                                             (set-buffer (window-buffer (minibuffer-selected-window)))
		                                             (or (thing-at-point 'symbol t) "")))))

;; Configure directory extension.
(keymap-set vertico-map "RET" #'vertico-directory-enter)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
(keymap-set vertico-map "M-DEL" #'vertico-directory-up)

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)

;;; marginalia
(marginalia-mode)

;;; orderless
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      orderless-component-separator #'orderless-escapable-split-on-space)

;;; consult
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(provide 'init-minibuffer)
