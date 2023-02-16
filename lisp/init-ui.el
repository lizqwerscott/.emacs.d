;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Jetbrains Mono" "Fira Code" "Source Code Pro"
                           "SF Mono" "Hack" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 180))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))


(setup-fonts)
(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

(use-package pretty-mode
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . pretty-mode))

(use-package ligature
  :quelpa (ligature :fetcher git :url "https://github.com/mickeynp/ligature.el.git")
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
  :ensure t
  :defer)

;; (use-package modus-themes
;;   :ensure t)

(use-package ef-themes
  :ensure t
  :defer)

(use-package flucui-themes
  :ensure t
  :defer)

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
;; (load-theme 'ef-summer t)
;; (flucui-themes-load-style 'dark)

(use-package circadian
  :ensure t
  :config
  ;; (setq calendar-latitude 37)
  ;; (setq calendar-longitude 112)
  ;; (setq circadian-themes '((:sunrise . ef-summer)
  ;;                          (:sunset  . solarized-dark)))
  (setq circadian-themes '(("8:00" . ef-day)
                           ("16:00" . solarized-dark)))
  (circadian-setup))

;;; Background
(setq default-frame-alist
      '((fullscreen . fullboth)
        (alpha-background . 100)))

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

;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (if (get-buffer "*Netease-Cloud-Music*")
;;                   (netease-cloud-music-add-header-lyrics))))

(use-package awesome-tray
  :quelpa (awesome-tray :fetcher git :url "https://github.com/manateelazycat/awesome-tray.git")
  :ensure t
  :hook (after-init . awesome-tray-mode)
  :custom
  (awesome-tray-active-modules
   '("location" "belong" "file-path" "mode-name" "date")
   "Lazycat config"))

(use-package sort-tab
  :quelpa (sort-tab :fetcher git :url "https://github.com/manateelazycat/sort-tab.git")
  :hook (after-init . sort-tab-mode))

;;; Icons
(use-package all-the-icons
  :ensure t
  :diminish t)

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;;; Git message
(use-package diff-hl
  :ensure t
  ;; :custom-face
  ;; (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  ;; (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  ;; (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  ;; :bind (:map diff-hl-command-map
  ;;        ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1))

  ;; Set fringe style
  ;; (setq-default fringes-outside-margins t)

  ;; (with-no-warnings
  ;;   (defun my-diff-hl-fringe-bmp-function (_type _pos)
  ;;     "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
  ;;     (define-fringe-bitmap 'my-diff-hl-bmp
  ;;       (vector (if sys/linuxp #b11111100 #b11100000))
  ;;       1 8
  ;;       '(center t)))
  ;;   (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  ;;   (unless (display-graphic-p)
  ;;     ;; Fall back to the display margin since the fringe is unavailable in tty
  ;;     (diff-hl-margin-mode 1)
  ;;     ;; Avoid restoring `diff-hl-margin-mode'
  ;;     (with-eval-after-load 'desktop
  ;;       (add-to-list 'desktop-minor-mode-table
  ;;                    '(diff-hl-margin-mode nil)))))

;; (use-package vc-msg
;;   :ensure t)

;; (use-package symbol-overlay
;;   :ensure t
;;   :hook (prog-mode . symbol-overlay-mode))

;;; Line number
(use-package emacs
  :unless sys/win32p
  :hook (((prog-mode text-mode) . display-line-numbers-mode))
  :config
  (setq display-line-numbers-type 'relative))

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
(use-package avy
  :ensure t
  :diminish t)

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((help-mode :select t :align t :size 0.4)
                   ("*Process List*" :select t :align t)
                   ("*One-Key*" :select t :align 'below))))

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

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config (setq so-long-threshold 400))

(provide 'init-ui)
;;; init-ui.el ends here.
