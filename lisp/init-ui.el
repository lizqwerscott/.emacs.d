;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

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
      '((alpha-background . 100)))

;;; Head line && Mode line && tab bar
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-buffer-file-name-style
;;         'file-name)
;;   (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)))

;; (use-package nyan-mode
;;   :ensure t
;;   :hook (doom-modeline-mode . nyan-mode))

;; Show the current function name in the header line
;; (which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))

;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (if (get-buffer "*Netease-Cloud-Music*")
;;                   (netease-cloud-music-add-header-lyrics))))

;; (require 'sort-tab)
;; (sort-tab-mode 1)
(require 'init-tab-bar)

;; (defun breadcrumb-info ()
;;   (breadcrumb--header-line))

;; (defface breadcrumb-face ()
;;   ""
;;   :group 'awesome-tray)

;; (add-to-list 'awesome-tray-module-alist
;; 	         '("breadcrumb" . (breadcrumb-info breadcrumb-face)))

(setq awesome-tray-date-format "%H:%M")
;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "file-path" "buffer-name" "mode-name" "battery" "git" "date")
;;         '("meow" "location" "file-path" "buffer-name" "mode-name" "git" "date")))

(setq awesome-tray-active-modules
      (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
          '("meow" "location" "buffer-name" "mode-name" "battery" "git" "date")
        '("meow" "location" "buffer-name" "mode-name" "git" "date")))


;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "breadcrumb" "mode-name" "battery" "date")
;;         '("meow" "location" "breadcrumb" "mode-name" "date")))


(when (display-graphic-p)
  (awesome-tray-mode))

(custom-set-faces
 '(header-line ((t (:inherit t :foreground unspecified :background unspecified)))))

(breadcrumb-mode)
;; (setq header-line-format nil)

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

;;; Dashboard
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
;; (use-package page-break-lines
;;   :ensure t
;;   :hook (dashboard-mode . page-break-lines-mode)
;;   :config
;;   (set-fontset-font "fontset-default"
;;                     (cons page-break-lines-char page-break-lines-char)
;;                     (face-attribute 'default :family)))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing")
;;   ;; (setq dashboard-banner-logo-title "LizqwerScott - Enjoy Yourself")
;;   ;; (setq dashboard-banner-logo-title "My name is God, God is me.")
;;   ;; (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-startup-banner "~/.emacs.d/logo.png")
;;   ;; (setq dashboard-startup-banner "~/Downloads/logo.png")
;;   (setq dashboard-page-separator "\n\f\f\n")
;;   (setq dashboard-center-content t)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-projects-backend 'project-el)
;;   (setq dashboard-items '((recents . 10)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda . 5)))
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-set-footer t)
;;   (setq dashboard-footer (format "Powered by Lizqwer Scott, %s" (format-time-string "%Y")))
;;   ;; (setq dashboard-footer-icon (or (all-the-icons-faicon "heart"
;;   ;;                                                       :height 1.1
;;   ;;                                                       :v-adjust -0.05
;;   ;;                                                       :face 'error)
;;   ;;                                 (propertize ">" 'face 'dashboard-footer)))
;;   (dashboard-setup-startup-hook)
;;   ;:hook ((after-init . dashboard-refresh-buffer))
;;   )

;;; Highlight
(require 'init-highlight)

;;; Window
(require 'ace-window)
(set-face-attribute 'aw-leading-char-face nil :height 400)
(add-to-list 'aw-ignored-buffers "*Ilist*")

(winner-mode 1)
(require 'shackle)
(shackle-mode)

(setq shackle-default-size 0.5)
(setq shackle-default-alignment 'below)
(setq shackle-rules
      '((help-mode :select t :align t :size 0.4)
        ("*Process List*" :select t :align t)
        ("*One-Key*" :select t :align 'below)
        ("*eshell*" :regexp t :select t :align 'below)))

;;; Another

;; (require 'zone)
;; (zone-when-idle 600)

(which-key-mode)
(global-so-long-mode 1)

;;; Holo layer
;; (setq holo-layer-show-place-info-p t)
;; (require 'holo-layer)
;; (setq holo-layer-enable-cursor-animation nil)
;; (setq holo-layer-hide-mode-line t)
;; (setq holo-layer-enable-window-border t)
;; (setq holo-layer-sort-tab-ui nil)
;; (holo-layer-enable)

;;; Click to browse URL or to send to e-mail address
(add-hook 'text-mode-hook
          'goto-address-mode)

(add-hook 'prog-mode-hook
          'goto-address-prog-mode)

(provide 'init-ui)
;;; init-ui.el ends here.
