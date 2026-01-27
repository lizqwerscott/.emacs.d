;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; theme
(require 'init-theme)

;;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(setq initial-frame-alist
      '((top . 0.5)
        (left . 0.5)
        (width . 0.9)
        (height . 0.9)))

(defun set-alpha-background (symbol value)
  "Set SYMBOL VALUE.
and update transparent."
  (set-default-toplevel-value symbol value)
  (setf (alist-get 'alpha-background default-frame-alist) value)
  (when-let* ((frame (selected-frame)))
    (set-frame-parameter frame 'alpha-background value)))

(defcustom user/alpha-background 100
  "Default alpha background."
  :group 'user
  :type 'number
  :set #'set-alpha-background)

(defun set-fullscreenp (symbol value)
  "Set SYMBOL VALUE."
  (set-default-toplevel-value symbol value)
  (when value
    (add-list-to-list 'initial-frame-alist
                      '((fullscreen . fullboth))))
  (let* ((frame (selected-frame))
         (fullscreen (frame-parameter frame 'fullscreen)))
    (if value
        (modify-frame-parameters frame `((fullscreen . fullboth) (fullscreen-restore . ,fullscreen)))
      (let ((fullscreen-restore (frame-parameter frame 'fullscreen-restore)))
	    (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	        (set-frame-parameter frame 'fullscreen fullscreen-restore)
	      (set-frame-parameter frame 'fullscreen nil))))))

(defcustom user/start-fullscreenp t
  "Is fullscreen in start."
  :group 'user
  :type 'boolean
  :set #'set-fullscreenp)

;;; modeline
(require 'init-modeline)

;;; tab bar
(require 'init-tab-bar)

;;; Head line
(custom-set-faces
 '(header-line ((t (:inherit t :foreground unspecified :background unspecified)))))

(setq breadcrumb-imenu-max-length 100)
(breadcrumb-mode)

(with-hook my/tramp-remote-file-hook
  (breadcrumb-local-mode -1))

;;; Line number
(unless sys/win32p
  (setq display-line-numbers-type 'relative)
  (add-hooks '(prog-mode)
             #'display-line-numbers-mode))

;;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen nil
      inhibit-startup-message t)

;;; Mouse & Smooth Scroll
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      auto-window-vscroll nil
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (pixel-scroll-mode t))

;;; Inhibit Mouse
(add-hook 'after-init-hook
          #'inhibit-mouse-mode)

;;; Logo
(setq fancy-splash-image user/logo)

;;; Dashboard
(require 'init-dashboard)

;;; Highlight
(require 'init-highlight)

;;; Window

(require 'init-window)

;;; Another

;; (require 'zone)
;; (zone-when-idle 600)
(global-so-long-mode 1)

;;; Click to browse URL or to send to e-mail address
(add-hook 'text-mode-hook
          'goto-address-mode)

(add-hook 'prog-mode-hook
          'goto-address-prog-mode)

;;; Imenu list
(with-eval-after-load 'imenu-list
  (add-to-list 'display-buffer-alist
               '("\\*Ilist\\*"
                 (imenu-list-display-buffer)
                 (window-parameters . ((no-delete-other-windows . t)
                                       (no-other-window . t))))))

;;; Redacted
;; Enable `read-only-mode' to ensure that we don't change what we can't read.
(add-hook 'redacted-mode-hook
          (lambda ()
            (read-only-mode
             (if redacted-mode 1 -1))))

;;; notification
(setq knockknock-border-color (face-foreground 'font-lock-keyword-face))

;;; project color
(autoload #'global-project-color-mode "project-color" nil t)
(global-project-color-mode)

(provide 'init-ui)
;;; init-ui.el ends here.
