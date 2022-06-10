;;theme

(set-frame-font "Source Code Pro 15")
(set-face-attribute 'default t :font "Source Code Pro 15")
;(set-face-attribute 'default nil :height 140)

(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola")))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(global-hl-line-mode 1)

(use-package gruvbox-theme
  :ensure t
  ;:init (load-theme 'gruvbox-dark-soft t)
  )

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

(use-package awesome-tray
  ;:hook (after-init . awesome-tray-mode)
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
  (setq dashboard-startup-banner 'official)
  (setq dashboard-page-separator "\n\f\n")
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
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column))

(provide 'init-ui)
