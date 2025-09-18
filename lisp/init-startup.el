;;; init-startup.el --- startup                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer noninteractive)

;;; Personal information
(setq user-full-name "lizqwer scott")
(setq user-mail-address "lizqwerscott@gmail.com")

;;; Emacs Config
(setq-default lexical-binding t)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(require 'no-littering)

(let ((path (concat user-emacs-directory "tmp")))
  (unless (file-directory-p path)
    (make-directory path))
  (setq temporary-file-directory
        path))

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
(setq column-number-mode t
      line-number-mode t
      kill-whole-line t               ; Kill line including '\n'
      line-move-visual nil
      track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
      set-mark-command-repeat-pop t)

(setq-default show-trailing-whitespace nil)

;; Prettify the process list
(with-no-warnings
  (defun my-list-processes--prettify ()
    "Prettify process list."
    (when-let* ((entries tabulated-list-entries))
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

;;; Indent
(setq tab-always-indent 'complete)

;;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default fill-column 80
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
      ring-bell-function 'ignore ; 禁止响铃
      )

(customize-set-variable 'kill-do-not-save-duplicates t)
(setq ad-redefinition-action 'accept)

(setq delete-old-versions t)
(setq create-lockfiles nil)

(setq confirm-kill-processes nil)

;; (setq truncate-partial-width-windows nil)

(setq mouse-yank-at-point t)

(repeat-mode)

;; 增加长行处理性能
;; (setq-default bidi-display-reordering nil) ; 完全关闭 bidi

;; from doom emacs
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; performance
;; Disable garbage collection when entering commands.
(setq garbage-collection-messages nil)  ;gc时显示消息
(setq byte-compile-warnings nil)    ;关闭字节编译警告

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000)))

;; Optimization
(when sys/win32p
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-use-native-image-API t         ; use native w32 API
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size 65536))       ; read more at a time (64K, was 4K)
(unless sys/macp
  (setq command-line-ns-option-alist nil))
(unless sys/linuxp
  (setq command-line-x-option-alist nil))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;; Imenu
(setq imenu-max-item-length 100)

;;; Bookmark
(setq bookmark-save-flag 1)

;;; Auto Revert
(setopt global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
(global-auto-revert-mode)

;;; Emoji
(setq emoji-alternate-names
      '(("😂" "haha")
        ("👍" "好" "hc" "hao")
        ("🧐" "?" "啊" "a")
        ("😭" "悲" "大哭" "bei" "bw")))

;;; exec path from shell
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH"))

;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
(setq exec-path-from-shell-check-startup-files nil) ;
(setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
(exec-path-from-shell-initialize)

(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode t)
            (recentf-mode t)))

(provide 'init-startup)
;;; init-startup.el ends here.
