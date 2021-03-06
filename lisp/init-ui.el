;;Font
(if *is-windows*
    (progn
      (set-face-attribute 'default nil :height 170)
      (dolist (charset '(han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "Microsoft Yahei UI" :size 17))))
  (progn
    (set-frame-font "Source Code Pro 18")
    (set-face-attribute 'default t :font "Source Code Pro 18")
    (when (member "Symbola" (font-family-list))
      (set-fontset-font "fontset-default" nil
                        (font-spec :size 18 :name "Symbola")))
    (when (member "Symbola" (font-family-list))
      (set-fontset-font t 'unicode "Symbola" nil 'prepend))))

(global-hl-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;Theme
(use-package gruvbox-theme
  :ensure t
  ;:init (load-theme 'gruvbox-dark-soft t)
  )

(use-package doom-themes
  :ensure t
  ;;:init (load-theme 'doom-one t)
  )

(use-package monokai-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(load-theme 'doom-one t)
;;(load-theme 'tango-dark t)
;;(load-theme 'monokai t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'vscode-dark-plus t)

(setq default-frame-alist '((width . 90)
                            (height . 50)
                            (alpha-background . 100)))

(use-package doom-modeline
  :ensure t
  ;;:hook (after-init . doom-modeline-mode)
  )
;;(setq-default header-line-format '(" %l %b " default-directory))
(use-package awesome-tray
  :hook (after-init . awesome-tray-mode)
  :custom
  (awesome-tray-active-modules
   '("location" "belong" "file-path" "mode-name" "git" "input-method" "flymake")
   "My tray config"))

(use-package sort-tab
  :hook (after-init . sort-tab-mode))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (setq git-gutter:hide-gutter t))

;; (use-package symbol-overlay
;;   :ensure t
;;   :hook (prog-mode . symbol-overlay-mode))

(use-package emacs
  :unless *is-windows*
  :config
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t))

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
  ;; (setq dashboard-banner-logo-title "My name is God, God is me.")
  (setq dashboard-startup-banner 'logo)
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

(use-package highlight-indent-guides
  :ensure t
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column))

(use-package indent-guide
  :ensure t
  :hook (after-init . indent-guide-global-mode)
  :config
  (set-face-background 'indent-guide-face
                       "dimgray")
  )

(use-package paren
  :ensure nil
  :hook (afte-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))

(use-package text-mode
  :ensure nil
  :custom
  (word-wrap-by-category t))

(require 'zone)
(zone-when-idle 600)

(provide 'init-ui)
