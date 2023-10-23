(setq user-full-name "lizqwer scott")
(setq user-mail-address "lizqwerscott@gmail.com")

;;; Emacs Config
(setq-default lexical-binding t)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(customize-set-variable 'kill-do-not-save-duplicates t)
(customize-set-variable 'auto-revert-interval 1)

(setq ad-redefinition-action 'accept)

(global-hl-line-mode 1)

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(pixel-scroll-mode t)
(pixel-scroll-precision-mode t)
(setq scroll-step 1
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

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
;; 增加长行处理性能
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)

(setq confirm-kill-processes nil)

(setq word-wrap-by-category t) ;按照中文折行

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

(setq-default tab-width 4
              indent-tabs-mode nil)

;; (global-auto-revert-mode 1)
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen nil
      inhibit-startup-message t)

(require 'no-littering)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))

;;; History
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(setq enable-recursive-minibuffers t  ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)
(savehist-mode 1)

(setq load-prefer-newer t)

(setq inhibit-compacting-font-caches t)

(setq auto-window-vscroll nil)
(setq truncate-partial-width-windows nil)

(setq mouse-yank-at-point t)

(setq-default fill-column 80)

(repeat-mode)

;;; Dired
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

(with-eval-after-load 'dired
  (setq dired-kill-when-opening-new-dired-buffer t))

;;(setq dired-omit-files
;;    (concat dired-omit-files "\\|^\\..*$"))

;;; Imenu
(setq imenu-max-item-length 100)
(setq breadcrumb-imenu-max-length 100)

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
;;         ("/" . dirvish-fd)
;;         ("a"   . dirvish-quick-access)
;;         ("f"   . dirvish-file-info-menu)
;;         ("y"   . dirvish-yank-menu)
;;         ("N"   . dirvish-narrow)
;;         ("^"   . dirvish-history-last)
;;         ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;         ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;         ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;         ("TAB" . dirvish-subtree-toggle)
;;         ("h" . dired-up-directory)
;;         ("q" . my/meow-quit))
;;   :config
;;   (dirvish-peek-mode))

(let ((path "~/.emacs.d/tmp/"))
  (when (not (file-directory-p path))
    (setq temporary-file-directory
          path)))

(defvar *start-banner* (propertize ";;     *
;;      May the Code be with You!
;;     .                                 .
;;                               *
;;          /\\/|_      __/\\\\
;;         /    -\\    /-   ~\\  .              \\='
;;         \\    = Y =T_ =   /
;;          )==*(\\=`     \\=`) ~ \\
;;         /     \\     /     \\
;;         |     |     ) ~   (
;;        /       \\   /     ~ \\
;;        \\       /   \\~     ~/
;; _/\\_/\\_/\\__  _/_/\\_/\\__~__/_/\\_/\\_/\\_/\\_/\\_
;; |  |  |  | ) ) |  |  | ((  |  |  |  |  |  |
;; |  |  |  |( (  |  |  |  \\\\ |  |  |  |  |  |
;; |  |  |  | )_) |  |  |  |))|  |  |  |  |  |
;; |  |  |  |  |  |  |  |  (/ |  |  |  |  |  |
;; |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
\n" 'face '(:foreground "green")))

;; 自定义 *scratch* 内容
;;;###autoload
(defun +evan/scratch-setup()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer "*scratch*")
      ;; (erase-buffer)
      (insert *start-banner*)
      (insert (format "启动时长: %s" (emacs-init-time)))
      (insert "\n")
      (insert-button "Quit Emacs"
		             'action (lambda (_button)
			                   (save-buffers-kill-emacs)))
      (insert "\n")
      ;; (insert "Recent Files\n")
      ;; (dolist (f recentf-list)
	  ;;   (insert-button f
	  ;;                  'action (lambda (region)
	  ;;   		                 (require 'f)
	  ;;   		                 (let* ((f (buffer-substring-no-properties (overlay-start region) (overlay-end region)))
	  ;;   			                    (fname (f-filename f)))
	  ;;   		                   (find-file-noselect f)
	  ;;   		                   (switch-to-buffer fname))))
	  ;;   (insert "\n"))
      ))
  (goto-char (point-max)))

(provide 'init-startup)
;;; init-startup.el ends here.
