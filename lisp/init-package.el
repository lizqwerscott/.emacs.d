(use-package restart-emacs
  :ensure t)

(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/activate))

;;(require 'init-company)

;; (use-package eglot
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'eglot-ensure)
;;   (add-hook 'cmake-mode-hook 'eglot-ensure)
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   (add-hook 'c++-mode-hook 'eglot-ensure)
;;   :config
;;   (progn
;;     (setq eldoc-echo-area-use-multiline-p 3
;;           eldoc-echo-area-display-truncation-message nil)
;;     (set-face-attribute 'eglot-highlight-symbol-face nil
;;                         :background "#b3d7ff")
;;     (add-to-list 'eglot-server-programs
;;                  '((c-mode c++-mode) . ("ccls")))
;;     (add-to-list 'eglot-server-programs
;;                  '(python-mode . ("jedi-language-server")))))

;;; Lsp bridge
(use-package lsp-bridge
  :bind
  (:map acm-mode-map ("TAB" . #'acm-select-next))
  (:map acm-mode-map ([backtab] . #'acm-select-prev))
  :hook (after-init . global-lsp-bridge-mode)
  :custom
  (acm-candidate-match-function #'orderless-literal))



(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/config/yasnippet/snippets/"))
  :hook (after-init . yas-global-mode))

(use-package common-lisp-snippets
  :ensure t
  :hook (common-lisp-mode . common-lisp-snippets-initialize))

(use-package flycheck
  :ensure t
  ;;:hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-disable-checker '(c/c++-clang))
  :config
  (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode org-mode diff-mode shell-mode eshell-mode)
        flycheck-emacs-lisp-load-path 'inherit))

(use-package flymake
  :ensure t
  :hook (after-init . flymake-mode)
  :config
  (setq flymake-run-in-place nil)
  (setq temporary-file-directory "~/.emacs.d/tmp/"))

(use-package cmake-mode
  :ensure t)

(use-package cmake-project
  :ensure t)

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (progn
    (setq evil-want-integration t)
    (setq evil-want-minibuffer nil)))

(use-package evil-leader
  :ensure t
  :config (progn
	    (evil-leader/set-leader "<SPC>")
	    (global-evil-leader-mode)))

(use-package evil-paredit
  :ensure t
  :after evil)

(add-hook 'paredit-mode-hook #'(lambda ()
                                 (evil-paredit-mode 1)))

(use-package sly
  :ensure t
  :hook (sly-mode . (lambda ()
                      (unless (sly-connected-p)
                        (save-excursion (sly)))))
  :config
  (setq sly-complete-symbol-function 'sly-simple-completions)
  ;;(setq sly-complete-symbol-function 'sly-flex-completions)
  (setq inferior-lisp-program "ccl"))

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)

(use-package paredit
  :ensure t)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(use-package magit
  :ensure t)

(use-package vc-msg
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (setq git-gutter:hide-gutter t))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode))

(setq browse-url-browser-function 'browse-url-chrome)

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

(use-package json-mode
  :ensure t
  :defer 2)

(use-package ox-hugo
  :ensure t
  :init
  (setq org-hugo-base-dir "~/MyProject/website/saveLife")
  (setq org-hugo-default-section-directory "zh-CN/post")
  :after ox)

(use-package format-all
  :ensure t)

;; format c++ or c
(defun format-this-buffer ()
  "Format this all buffer."
  (interactive "")
  (if (or (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (shell-command (concat "astyle "
                             (buffer-file-name)))))

;; code hide
(add-hook 'c-mode-hook
          'hs-minor-mode)

(add-hook 'c++-mode-hook
          'hs-minor-mode)

(add-hook 'emacs-lisp-mode-hook
          'hs-minor-mode)

(add-hook 'python-mode-hook
          'hs-minor-mode)

(use-package focus
  :ensure t)

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :custom
  (eaf-browser-continue-where-left-off t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser))

(require 'eaf)
(require 'eaf-browser)
(require 'eaf-evil)
(require 'eaf-pdf-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-git)
(require 'eaf-mindmap)
(require 'eaf-demo)
;;(require 'eaf-mermaid)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(provide 'init-package)
;;; init-package.el ends here
