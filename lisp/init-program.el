;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:


;;; Ctags

;; (use-package citre
;;   :ensure t)

;;; Lsp Server
;; (require 'init-eglot)

;;; check error
;; flymake
(add-hook 'after-init-hook
          #'flymake-mode)
(setq flymake-run-in-place nil)

;;; debug
(require 'init-dap)

;;; language
(require 'init-elisp)
(require 'init-python)
(require 'init-haskell)
(require 'init-c++)
(require 'init-web)
(require 'init-common-lisp)
(require 'init-rust)
(require 'init-sql)
(require 'init-go)


(provide 'init-program)
;;; init-program.el ends heres.
