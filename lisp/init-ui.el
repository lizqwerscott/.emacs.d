;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Font

(require 'init-font)

;;; Theme

(require 'init-theme)

;;; Background
(setq default-frame-alist
      '((fullscreen . fullboth)
        (alpha-background . 100)))

;;; UI
(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))
(global-hl-line-mode 1)
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

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
(require 'all-the-icons-completion)
(add-hook 'marginalia-mode-hook
          #'all-the-icons-completion-marginalia-setup)
(all-the-icons-completion-mode)

;;; Git message
(require 'diff-hl)
(global-diff-hl-mode)
(global-diff-hl-show-hunk-mouse-mode)
(add-hook 'dired-mode
          'diff-hl-dired-mode)

(setq diff-hl-draw-borders nil)
(diff-hl-flydiff-mode)

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

;;; hl indetn
;; (use-package indent-guide
;;   :ensure t
;;   :hook ((prog-mode . indent-guide-mode)))
(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'highlight-indent-guides)
              (setq highlight-indent-guides-auto-odd-face-perc 50)
              (setq highlight-indent-guides-auto-even-face-perc 50)
              (highlight-indent-guides-mode 1)))

;;; Paren
(require 'paren)
(show-paren-mode 1)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(setq show-paren-style 'parenthesis
      show-paren-context-when-offscreen 'overlay)


;;; Highlight
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'rainbow-delimiters)
               (rainbow-delimiters-mode 1)))

(setq color-identifiers:recoloring-delay 1)
(add-hook 'prog-mode-hook
          'color-identifiers-mode)

(add-hook 'web-mode-hook
          #'(lambda ()
              (require 'highlight-matching-tag)
              (highlight-matching-tag 1)))

(require 'init-hl-todo)

;; (use-package highlight-defined
;;   :ensure t
;;   ;; :hook (elisp-lisp-mode . highlight-defined-mode)
;;   )

;;; Window
(require 'shackle)
(shackle-mode)

(setq shackle-default-size 0.5)
(setq shackle-default-alignment 'below)
(setq shackle-rules '((help-mode :select t :align t :size 0.4)
                      ("*Process List*" :select t :align t)
                      ("*One-Key*" :select t :align 'below)))

;;; Another

;; (require 'zone)
;; (zone-when-idle 600)

(which-key-mode)
(global-so-long-mode 1)

;;; Holo layer
;; (setq holo-layer-show-place-info-p t)
;; (require 'holo-layer)
;; (setq holo-layer-enable-cursor-animation t)
;; (setq holo-layer-hide-mode-line t)
;; (holo-layer-enable)

(provide 'init-ui)
;;; init-ui.el ends here.
