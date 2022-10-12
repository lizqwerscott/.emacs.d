;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords:

;;; Commentary:

;;; Code:

;;; Font
(if *is-windows*
    (progn
      (set-face-attribute 'default nil :height 170)
      (dolist (charset '(han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "Microsoft Yahei UI" :size 17))))
  (progn
    ;; (set-frame-font "Source Code Pro 18")
    ;; (set-face-attribute 'default t :font "Source Code Pro 18")
    (set-frame-font "Fira Code 18")
    (set-face-attribute 'default t :font "Fira Code 18")
    ;; (when (member "Symbola" (font-family-list))
    ;;   (set-fontset-font "fontset-default" nil
    ;;                     (font-spec :size 18 :name "Symbola")))
    ;; (when (member "Symbola" (font-family-list)) (set-fontset-font t 'unicode "Symbola" nil 'prepend))
    (when (member "霞鹜文楷" (font-family-list))
      (set-fontset-font 'fontset-default nil
                        (font-spec :size 18 :name "霞鹜文楷")))))

(use-package ligature
  :hook
  (after-init . global-ligature-mode)
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

;;; Theme
;; (use-package gruvbox-theme
;;   :ensure t)

;; (use-package doom-themes
;;   :ensure t)

;; (use-package monokai-theme
;;   :ensure t)

(use-package solarized-theme
  :ensure t)

;; (use-package modus-themes
;;   :ensure t)

(use-package ef-themes
  :ensure t)

(use-package flucui-themes
  :ensure t)

;; (load-theme 'gruvbox-dark-soft t)
;; (load-theme 'doom-one t)
;;(load-theme 'tango-dark t)
;; (load-theme 'monokai t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'vscode-dark-plus t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'ef-summer t)
;; (load-theme 'modus-operandi t)
;; (require 'lazycat-theme)
;; (lazycat-theme-load-dark)
;; (load-theme 'ef-night t)
;; (load-theme 'ef-day t)
(load-theme 'ef-summer t)
;; (flucui-themes-load-style 'dark)

;;; Background
(setq default-frame-alist
      '((width . 90)
        (height . 50)
        (alpha-background . 90)))

;;; UI
(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))
(global-hl-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(add-hook 'after-init-hook
          #'(lambda ()
              (if (get-buffer "*Netease-Cloud-Music*")
                  (netease-cloud-music-add-header-lyrics))))

(use-package awesome-tray
  :hook (after-init . awesome-tray-mode)
  :custom
  (awesome-tray-active-modules
   '( "git" "mode-name" "location" "flymake" "date")
   "My tray config"))

(use-package sort-tab
  :hook (after-init . sort-tab-mode))

;;; Icons
(require-package 'all-the-icons)

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;;; Git message
;; (use-package git-gutter
;;   :ensure t
;;   :config
;;   (global-git-gutter-mode +1)
;;   (setq git-gutter:modified-sign " ")
;;   (setq git-gutter:added-sign "+")
;;   (setq git-gutter:deleted-sign "-")
;;   (setq git-gutter:hide-gutter t))

;; (use-package vc-msg
;;   :ensure t)

;; (use-package symbol-overlay
;;   :ensure t
;;   :hook (prog-mode . symbol-overlay-mode))

;;; Line number
(use-package emacs
  :unless *is-windows*
  :hook (((prog-mode text-mode) . display-line-numbers-mode))
  :config
  (setq display-line-numbers-type 'relative))

;;; Dashboard
(use-package page-break-lines
  :ensure t
  :hook (dashboard-mode . page-break-lines-mode)
  :config
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing")
  ;; (setq dashboard-banner-logo-title "LizqwerScott - Enjoy Yourself")
  ;; (setq dashboard-banner-logo-title "My name is God, God is me.")
  ;; (setq dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner "~/.emacs.d/logo.png")
  ;; (setq dashboard-startup-banner "~/Downloads/logo.png")
  (setq dashboard-page-separator "\n\f\f\n")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer t)
  (setq dashboard-footer (format "Powered by Lizqwer Scott, %s" (format-time-string "%Y")))
  ;; (setq dashboard-footer-icon (or (all-the-icons-faicon "heart"
  ;;                                                       :height 1.1
  ;;                                                       :v-adjust -0.05
  ;;                                                       :face 'error)
  ;;                                 (propertize ">" 'face 'dashboard-footer)))
  (dashboard-setup-startup-hook)
  ;:hook ((after-init . dashboard-refresh-buffer))
  )

;;; hl indetn
(use-package indent-guide
  :ensure t
  :hook ((prog-mode . indent-guide-mode)))

;;; Paren
(use-package paren
  :ensure nil
  :hook (afte-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))

;;; Window
;; (use-package avy
;;   :ensure t)
(require-package 'avy)

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((help-mode :select t :align t :size 0.4)
                   ("*quickrun*" :select t :align t :size 0.4)
                   ("*Process List*" :select t :align t))))

;;; Another
(use-package text-mode
  :ensure nil
  :custom
  (word-wrap-by-category t))

;; (require 'zone)
;; (zone-when-idle 600)

;; (use-package highlight-defined
;;   :ensure t
;;   ;; :hook (elisp-lisp-mode . highlight-defined-mode)
;;   )

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config (setq so-long-threshold 400))

(provide 'init-ui)
;;; init-ui.el ends here.
