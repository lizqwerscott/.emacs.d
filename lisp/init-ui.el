;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; theme
(+lizqwer/load-theme user/night-theme)

;;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(setq default-frame-alist
      `((alpha-background . ,(if user/start-transparent
                                 90
                               100))
        ;; (fullscreen . maximized)
        ))

(setq initial-frame-alist
      '((top . 0.5)
        (left . 0.5)
        (width . 0.9)
        (height . 0.9)
        ;; (fullscreen . maximized)
        ))

(when user/start-fullscreen
  (unless sys/macp
    (toggle-frame-fullscreen)))

;;; modeline
(require 'init-modeline)

;;; tab bar
(require 'init-tab-bar)

;;; Head line
(custom-set-faces
 '(header-line ((t (:inherit t :foreground unspecified :background unspecified)))))

(setq breadcrumb-imenu-max-length 100)
(breadcrumb-mode)

;;; Line number
(unless sys/win32p
  (add-hooks '(prog-mode text-mode conf-mode)
             #'(lambda ()
                 (setq display-line-numbers-type 'relative)
                 (display-line-numbers-mode 1))))

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

;;; Logo
(setq fancy-splash-image user/logo)

;;; Dashboard
(require 'init-dashboard)

;;; Child frame
(require 'init-posframe)

;;; Highlight
(require 'init-highlight)

;;; Window

(require 'init-window)

;;; Buffer Name
(require 'buffer-name-relative)
(setq buffer-name-relative-prefix '("<" . "> ")
      buffer-name-relative-fallback 'default)

(defun buffer-name-relative-root-path-from-project (filepath)
  "Return the PROJECT directory from FILEPATH or nil."
  (let ((result nil))
    (when (fboundp 'project-root)
      (let ((dir (if (file-directory-p filepath)
                     (directory-file-name filepath)
                   (file-name-directory filepath))))
        (when (and dir (project-current nil dir))
          (condition-case-unless-debug err
              (setq result (project-root (project-current nil dir)))
            (error (message "Error finding PROJECT root name: %s" err))))))
    result))
(setq buffer-name-relative-root-functions '(buffer-name-relative-root-path-from-project))

(add-hook 'after-init-hook
          #'buffer-name-relative-mode)

;;; Another

;; (require 'zone)
;; (zone-when-idle 600)

(which-key-mode)
(global-so-long-mode 1)

;;; Click to browse URL or to send to e-mail address
(add-hook 'text-mode-hook
          'goto-address-mode)

(add-hook 'prog-mode-hook
          'goto-address-prog-mode)

;;; Imenu list
(require 'init-imenu-list)

;;; Eww image slice
(require 'init-image-slicing)

;;; Redacted
;; Enable `read-only-mode' to ensure that we don't change what we can't read.
(add-hook 'redacted-mode-hook
          (lambda ()
            (read-only-mode
             (if redacted-mode 1 -1))))

(provide 'init-ui)
;;; init-ui.el ends here.
