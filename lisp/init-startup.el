;;; init-startup.el --- init startup need packages   -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords:
;;; Commentary:

;;; Code:

;;; User config

(setq user-full-name "lizqwer scott")
(setq user-mail-address "lizqwerscott@gmail.com")

;;; Emacs Config
(setq-default lexical-binding t)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(customize-set-variable 'kill-do-not-save-duplicates t)
(setq auto-revert-interval 1)

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq default-buffer-file-coding-system 'utf-8)

(setq delete-old-versions t)
(setq make-backup-files nil
      auto-save-default nil)
(setq create-lockfiles nil)

(setq-default bidi-display-reordering nil)

(setq confirm-kill-processes nil)

;;; performance
;; Disable garbage collection when entering commands.
(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 800000))

;; (electric-pair-mode nil)
(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)

(setq-default indent-tabs-mode nil)

(global-auto-revert-mode 1)
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t)

(require-package 'no-littering)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))

(require 'no-littering)


;;; History
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(add-hook 'after-init-hook 'save-place-mode)
;; (use-package saveplace
;;   :ensure nil
;;   :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t  ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

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

(setq mouse-yank-at-point nil)

(setq-default fill-column 80)

(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?_ "w")))

(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?- "w")))

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
  ;; (setq enable-recursive-minibuffers t)
  )

;;; Minibuffer
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind
  (:map vertico-map ("M-TAB" . #'minibuffer-complete)))

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

(require-package 'consult-project-extra)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; compleations
(require-package 'affe)

(require 'init-corfu)

;;; Dired
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

;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-mode-line-format
;;    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   ;; (dirvish-attributes '(subtree-state all-the-icons vc-state git-msg))
;;   (dirvish-attributes '(all-the-icons vc-state file-size))
;;   (dirvish-preview-dispatchers '(vc-diff))
;;   (dirvish-mode-line-height 0)
;;   (dirvish-header-line-height 0)
;;   :bind
;;   (:map dirvish-mode-map
;;         ("h" . dired-up-directory))
;;   :config
;;   (dirvish-peek-mode))

(let ((path "~/.emacs.d/tmp/"))
  (when (not (file-directory-p path))
    (setq temporary-file-directory
          "~/.emacs.d/tmp/")))

;;; Tramp

(setq recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG")
      tramp-auto-save-directory temporary-file-directory
      backup-directory-alist (list (cons tramp-file-name-regexp nil)))

(setq tramp-default-method "ssh")

(provide 'init-startup)
;;; init-startup.el ends here.
