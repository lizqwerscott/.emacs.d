;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; snippet

;; (use-package yasnippet
;;   :ensure t
;;   :hook (after-init . yas-global-mode)
;;   :config
;;   (setq yas-snippet-dirs
;;         '("~/.emacs.d/config/yasnippet/snippets/"))
;;   )

;; (use-package tempel
;;   :ensure t
;;   :bind
;;   (:map tempel-map
;;         ("TAB" . tempel-next))
;;   :config
;;   (setq tempel-path
;;         "~/.emacs.d/config/tempel/templates"))


;;(require 'init-company)

;; Ctags

;; (use-package citre
;;   :ensure t)

;;; Lsp Server

;;; check error

;; (use-package flycheck
;;   :ensure t
;;   :hook (after-init . global-flycheck-mode)
;;   :custom
;;   (flycheck-disable-checker '(c/c++-clang))
;;   :config
;;   (setq flycheck-global-modes '(not python-mode c++-mode c-mode text-mode outline-mode fundamental-mode org-mode diff-mode shell-mode eshell-mode)
;;   ;;       flycheck-emacs-lisp-load-path 'inherit)
;;   ;; (setq flycheck-global-modes '(not c++-mode c-mode text-mode outline-mode fundamental-mode org-mode diff-mode shell-mode eshell-mode)
;;         flycheck-emacs-lisp-load-path 'inherit)
;;   (setq flycheck-clang-language-standard "c++17"))

;; (use-package flycheck-rust
;;   :ensure t)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package consult-flycheck
;;   :ensure t)

(use-package flymake
  :ensure t
  :hook (after-init . flymake-mode)
  :config
  (setq flymake-run-in-place nil))

(provide 'init-program)
;;; init-program.el ends heres.
