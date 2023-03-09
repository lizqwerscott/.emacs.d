;;; init-tool.el --- init tool packages              -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Look require cl package
(defun check-package-use-cl ()
  "Check package is use cl."
  (interactive)
  (require 'loadhist)
  (file-dependents (feature-file 'cl)))

(use-package restart-emacs
  :ensure t
  :diminish t)

;; (use-package benchmark-init
;;   :ensure t
;;   :hook (after-init . benchmark-init/activate))

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000))

(use-package posframe
  :ensure t
  :diminish t)

(use-package separedit
  :ensure t
  :bind
  (:map prog-mode-map
        ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'org-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package focus
  :ensure t
  :diminish)

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(use-package go-translate
  :ensure t
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-noprompt-picker)
         :engines (list (gts-bing-engine))
         :render (gts-posframe-pop-render))))

;; Chinese-English translation popup
(require 'popweb-dict)

(use-package vterm
  :ensure t
  :diminish t)

(use-package vterm-toggle
  :ensure t
  :bind
  ("M-m" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(require 'vterm-toggle)

(defvar vterm-run-buffer nil)
(defun vterm-run (run-command)
  "Run the program including the current buffer in `vterm'."
  (interactive)
  (let* ((w (vterm-toggle--get-window)))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (with-current-buffer (vterm-toggle-cd)
            (setq vterm-run-buffer (current-buffer))
            (rename-buffer "*vterm run*")
            (compilation-shell-minor-mode 1)
            (vterm-send-M-w)
            (vterm-send-string run-command t)
            (vterm-send-return)))))

(defvar vterm-compile-buffer nil)
(defun vterm-compile ()
  "Compile the program including the current buffer in `vterm'."
  (interactive)
  (let* ((command (eval compile-command))
         (w (vterm-toggle--get-window)))
    (setq compile-command (compilation-read-command command))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (let ((root (project-root (project-current))))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (when root
            (vterm-send-string (concat "cd "
                                       root)
                               t)
            (vterm-send-return))
          (vterm-send-string compile-command t)
          (vterm-send-return))))))

(use-package xclip
  :ensure t
  :hook (after-init . xclip-mode))

(use-package auto-save
  :quelpa (auto-save :fetcher github :repo "manateelazycat/auto-save")
  :ensure t
  :hook
  (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(use-package markdown-mode
  :ensure t
  :diminish t)

;; Nice writing
(use-package olivetti
  :ensure t
  :diminish
  :init (setq olivetti-body-width 0.618))

;; eaf
(require 'eaf)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-git)
(require 'eaf-image-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-file-manager)

(eaf-bind-key meow-keypad "SPC" eaf-pdf-viewer-keybinding)

(setq eaf-proxy-type "http")
(setq eaf-proxy-host user/proxy-host)
(setq eaf-proxy-port user/proxy-rule-port)
(setq eaf-webengine-default-zoom 1.25)

(use-package request
  :ensure t)

;(require 'netease-cloud-music)
;(require 'netease-cloud-music-ui)

(use-package websocket
  :ensure t)

(use-package deno-bridge
  :quelpa (deno-bridge :fetcher git :url "https://github.com/manateelazycat/deno-bridge.git")
  :ensure t)

;; log
(use-package interaction-log
  :ensure t)

;;some tool function

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun tianqi ()
  "获取天气."
  (interactive)
  (eww "zh-cn.wttr.in/"))

(use-package google-this
  :ensure t
  :hook (after-init . google-this-mode))

;;; tree sitter
(when sys/linuxp
  (use-package tree-sitter
    :ensure t
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook
              #'tree-sitter-hl-mode))
  (use-package tree-sitter-langs
    :ensure t))

;;; telega
(when (cl-search "arch" operating-system-release)
  (use-package telega
    :quelpa (telega :fetcher github
                    :repo "zevlg/telega.el"
                    :branch "master"
                    :files (:defaults "contrib" "etc" "server" "Makefile"))
    :hook (telega-load . telega-notifications-mode)
    :config
    (setq telega-proxies
          `((:server ,user/proxy-host :port ,user/proxy-all-port :enable t
                     :type (:@type "proxyTypeSocks5"))))
    (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))))

;;; use crow translation
(require 'insert-translated-name)
(setq insert-translated-name-crow-engine "lingva")

(provide 'init-tool)
;;; init-tool.el ends here.
