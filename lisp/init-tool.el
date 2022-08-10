;;; init-tool.el --- init tool packages              -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwer@lzb>
;; Keywords: processes

;;; Commentary:

;;; Code:

(use-package restart-emacs
  :ensure t)

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
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode))

(use-package focus
  :ensure t)

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package request
  :ensure t)

(require 'netease-cloud-music)
(require 'netease-cloud-music-ui)

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

(use-package vterm
 :ensure t)

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

;; (use-package magit
;;   :ensure t)

(use-package xclip
  :ensure t
  :hook (after-init . xclip-mode))

(use-package auto-save
  :hook
  (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(use-package markdown-mode
  :ensure t)

(use-package posframe
  :ensure t)

(provide 'init-tool)
;;; init-tool.el ends here.
