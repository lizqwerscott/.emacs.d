;;; early init
(setq load-prefer-newer noninteractive)

;;; Personal information
(setq user-full-name "lizqwer scott")
(setq user-mail-address "lizqwerscott@gmail.com")

;;; Emacs Config
(setq-default lexical-binding t)

(require 'no-littering)
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))

(let ((path "~/.emacs.d/tmp/"))
  (when (not (file-directory-p path))
    (setq temporary-file-directory
          path)))

;;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq default-buffer-file-coding-system 'utf-8)

;;; History
(require 'recentf)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks" "bookmark-default"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir))))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(push (expand-file-name recentf-save-file) recentf-exclude)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

;;; savehist
(setq enable-recursive-minibuffers t  ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)
(savehist-mode 1)

;;; saveplace
(setq save-place-forget-unreadable-files nil)

;;; Simple
;; Prettify the process list
(with-no-warnings
  (defun my-list-processes--prettify ()
    "Prettify process list."
    (when-let ((entries tabulated-list-entries))
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (when-let* ((val (cadr (assoc p entries)))
                    (name (aref val 0))
                    (pid (aref val 1))
                    (status (aref val 2))
                    (status (list status
                                  'face
                                  (if (memq status '(stop exit closed failed))
                                      'error
                                    'success)))
                    (buf-label (aref val 3))
                    (tty (list (aref val 4) 'face 'font-lock-doc-face))
                    (thread (list (aref val 5) 'face 'font-lock-doc-face))
                    (cmd (list (aref val 6) 'face 'completions-annotations)))
          (push (list p (vector name pid status buf-label tty thread cmd))
		        tabulated-list-entries)))))
  (advice-add #'list-processes--refresh :after #'my-list-processes--prettify))

;;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'tex-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t ;按照中文折行
      )

(customize-set-variable 'kill-do-not-save-duplicates t)
(customize-set-variable 'auto-revert-interval 1)
(setq ad-redefinition-action 'accept)

(setq delete-old-versions t)
(setq create-lockfiles nil)

(setq confirm-kill-processes nil)

;; (setq truncate-partial-width-windows nil)

(setq mouse-yank-at-point t)

(repeat-mode)

;; 增加长行处理性能
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)

;;; performance
;; Disable garbage collection when entering commands.
(setq garbage-collection-messages t)	;gc时显示消息
(setq byte-compile-warnings nil)	;关闭字节编译警告

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000)))

;;; Imenu
(setq imenu-max-item-length 100)
(setq breadcrumb-imenu-max-length 100)

;;; Sqlite
(when (fboundp 'sqlite-open)
  (require 'emacsql-sqlite-builtin))

;;; vc
(setq vc-handled-backends '(Git))

;;; exec path from shell
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH"))

;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
(setq exec-path-from-shell-check-startup-files nil) ;
(setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
(exec-path-from-shell-initialize)

(provide 'init-startup)
;;; init-startup.el ends here.
