;;; init-tool.el --- init tool packages              -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes

;;; Commentary:

;;; Code:

;;; Look require cl package
(defun check-package-use-cl ()
  (interactive)
  (require 'loadhist)
  (file-dependents (feature-file 'cl)))

(require-package 'restart-emacs)

(use-package benchmark-init
  :ensure t :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/activate))

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000))

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

(require-package 'focus)

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(require-package 'focus)

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
         :engines (list (gts-bing-engine)
                        (gts-google-engine))
         :render (gts-posframe-pop-render))))

(require-package 'vterm)

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

(use-package xclip
  :ensure t
  :hook (after-init . xclip-mode))

(use-package auto-save
  :ensure nil
  :hook
  (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(require-package 'markdown-mode)

(require-package 'posframe)

;; Nice writing
(use-package olivetti
  :ensure t
  :diminish
  :init (setq olivetti-body-width 0.618))

;; eaf
(require 'eaf)
(require 'eaf-pdf-viewer)
(require 'eaf-git)
(require 'eaf-browser)

(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "20172")
(setq eaf-webengine-default-zoom 1.25)

(use-package request
  :ensure t)

(require 'popon)

;(require 'netease-cloud-music)
;(require 'netease-cloud-music-ui)

(use-package websocket
  :ensure t)

(require 'deno-bridge)

(provide 'init-tool)
;;; init-tool.el ends here.
