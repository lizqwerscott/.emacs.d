;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)

(electric-pair-mode)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(require 'init-awesome-pair)
(require 'init-fingertip)

(require 'aggressive-indent)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'c-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'go-ts-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'rust-ts-mode)
(add-hook 'emacs-lisp-mode-hook
          'aggressive-indent-mode)

(add-hook 'lisp-mode-hook
          'aggressive-indent-mode)

(add-hook 'python-mode-hook
          'aggressive-indent-mode)

;; (global-aggressive-indent-mode 1)

(require 'indent-yank)
(add-hook 'python-mode-hook 'indent-yank-mode)
(add-hook 'python-ts-mode-hook 'indent-yank-mode)

(provide 'init-edit)
;;; init-edit.el ends here.
