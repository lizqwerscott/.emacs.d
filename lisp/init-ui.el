;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; UI
;; (setq frame-resize-pixelwise t)
;; (dotimes (n 3)
;;   (toggle-frame-maximized))

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


(breadcrumb-mode)
(setq header-line-format nil)

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

(setq awesome-tray-active-modules
      (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
          '("meow" "file-path" "buffer-name" "mode-name" "battery" "date")
        '("meow" "file-path" "buffer-name" "mode-name" "date")))

;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "breadcrumb" "mode-name" "battery" "date")
;;         '("meow" "location" "breadcrumb" "mode-name" "date")))


(when (display-graphic-p)
  (awesome-tray-mode))

;;; Icons
(add-hook 'marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode)

;;; Line number
(unless sys/win32p
  (add-hooks '(prog-mode text-mode conf-mode)
             #'(lambda ()
                 (setq display-line-numbers-type 'relative)
                 (display-line-numbers-mode 1))))

;;; Dashboard
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

(add-hook 'prog-mode-hook
          'rainbow-mode)

;;; Window
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

(add-hook 'prog-mode
          'goto-address-prog-mode)

(provide 'init-ui)
;;; init-ui.el ends here.
