
(fset 'yes-or-no-p 'y-or-n-p)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

(setq delete-old-versions t)
(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq default-buffer-file-coding-system 'utf-8)
(setq auto-save-default nil)

(electric-pair-mode nil)
(setq-default indent-tabs-mode nil)

(global-auto-revert-mode 1)
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t)

(use-package no-littering
  :ensure t)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))

(require 'no-littering)

(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(use-package saveplace
  :ensure t
  :hook (after-init . (lambda () (save-place-mode t))))

(setq load-prefer-newer t)

(setq inhibit-compacting-font-caches nil)

(setq scroll-step 2
      scroll-margin 6
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

(setq auto-window-vscroll nil)
(setq truncate-partial-width-windows nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(setq mouse-yank-at-point nil)

(setq-default fill-column 80)

(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?_ "w")))

(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?- "w")))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config (which-key-mode 1))

(use-package ace-window
  :ensure t
  :init
  :bind ("M-o" . 'ace-window))

(use-package xclip
  :ensure t
  :hook (after-init . xclip-mode))

(use-package posframe
  :ensure t)

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind
  (:map vertico-map ("M-TAB" . #'minibuffer-complete)))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  ;:hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<")
  (consult-preview-at-point-mode))

(use-package consult-project-extra
  :ensure t)

(use-package affe
  :ensure t)

(require 'init-corfu)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-state 1)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-state 1)))

(use-package dired
  :config
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwin-target t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"))

(use-package dired-x
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

(use-package dirvish
  :ensure t
  :custom
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(subtree-state all-the-icons vc-state git-msg))
  (dirvish-preview-dispatchers '(vc-diff))
  ;; (dirvish-mode-line-height 0)
  ;; (dirvish-header-line-height 0)
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode))

(provide 'init-startup)
