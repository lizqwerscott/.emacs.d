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

(keymap-sets minibuffer-local-map
             '(("M-s" . consult-history)
               ("M-r" . consult-history)
               ("C-i" . (lambda ()
                          "Insert the currunt symbol."
                          (interactive)
                          (insert (save-excursion
                                    (set-buffer (window-buffer (minibuffer-selected-window)))
                                    (or (thing-at-point 'symbol t) "")))))))
;; Configure directory extension.
(keymap-sets vertico-map
             '(("RET" . vertico-directory-enter)
               ("DEL" . vertico-directory-delete-char)
               (("M-DEL" "s-DEL") . vertico-directory-up)
               ("s-RET" . vertico-exit-input)))

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)
;;; marginalia
(marginalia-mode)
(add-hook 'marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode)

;;; orderless
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      orderless-component-separator #'orderless-escapable-split-on-space)

;;; fussy
(push 'fussy completion-styles)
(setq
 ;; For example, project-find-file uses 'project-files which uses
 ;; substring completion by default. Set to nil to make sure it's using
 ;; flx.
 completion-category-defaults nil
 completion-category-overrides nil)
;; flx-rs
(require 'flx-rs)
(setq fussy-score-fn 'fussy-flx-rs-score)
(flx-rs-load-dyn)

;;; consult
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(setq consult-buffer-sources
      '(consult--source-hidden-buffer
        consult--source-modified-buffer
        consult--source-buffer
        ;; consult--source-recent-file
        ;; consult--source-file-register
        ;; consult--source-bookmark
        consult--source-project-buffer-hidden
        ;; consult--source-project-recent-file-hidden
        ))

;;; embark
(require 'init-embark)

(provide 'init-minibuffer)
