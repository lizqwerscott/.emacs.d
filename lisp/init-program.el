;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes.

;;; Commentary:


;;; Code:

;;(require 'init-company)

;;; Lsp Server

;; Eglot
;; (use-package eglot
;;   :ensure t
;;   :hook
;;   (cmake-mode-hook 'eglot-ensure)
;;   :init
;;   ;; (add-hook 'python-mode-hook 'eglot-ensure)
;;   (add-hook 'cmake-mode-hook 'eglot-ensure)
;;   (add-hook 'web-mode-hook 'eglot-ensure)
;;   (add-hook 'vue-mode-hook 'eglot-ensure)
;;   ;; (add-hook 'c-mode-hook 'eglot-ensure)
;;   ;; (add-hook 'c++-mode-hook 'eglot-ensure)
;;   :config
;;   (progn
;;     (setq eldoc-echo-area-use-multiline-p 3
;;           eldoc-echo-area-display-truncation-message nil)
;;     ;; (set-face-attribute 'eglot-highlight-symbol-face nil
;;     ;;                     :background "#b3d7ff")
;;     (add-to-list 'eglot-server-programs
;;                  '((c-mode c++-mode) . ("ccls")))
;;     (add-to-list 'eglot-server-programs
;;                  '(python-mode . ("jedi-language-server")))
;;     (add-to-list 'eglot-server-programs
;;                  '(vue-mode . "vls"))))

;; Lsp bridge
(use-package lsp-bridge
  :bind
  ;; (:map acm-mode-map ("TAB" . #'acm-select-next))
  ;; (:map acm-mode-map ([backtab] . #'acm-select-prev))
  (:map acm-mode-map ("C-n" . #'acm-select-next))
  (:map acm-mode-map ("C-p" . #'acm-select-prev))
  :hook (after-init . global-lsp-bridge-mode)
  ;; (((c++-mode . c-mode) . lsp-bridge-mode)
  ;;  (elisp-mode . lsp-bridge-mode))

  :custom
  (lsp-bridge-c-lsp-server "ccls")
  ;; (acm-enable-tabnine-helper t)
  ;; (lsp-bridge-enable-debug t)
  (lsp-bridge-python-lsp-server "jedi")
  ;; (acm-candidate-match-function 'orderless-regexp)
  :config
  ;; (setq lsp-bridge-default-mode-hooks
  ;;       (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
  (setq acm-enable-doc t))

;; (use-package dumb-jump
;;   :ensure t)
(require-package 'dumb-jump)

(require 'xref)
(require 'lsp-bridge)
(defun find-definition-with-lsp-bridge ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition))
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (current-word)))
      (funcall #'xref-find-definitions symb)))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun return-find-def ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (call-interactively 'xref-go-back))
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

;;; snippet

;; (use-package yasnippet
;;   :ensure t
;;   :hook (after-init . yas-global-mode)
;;   :config
;;   (setq yas-snippet-dirs
;;         '("~/.emacs.d/config/yasnippet/snippets/"))
;;   )

;; (use-package common-lisp-snippets
;;   :ensure t
;;   :hook (common-lisp-mode . common-lisp-snippets-initialize)
;;   :hook (after-init . common-lisp-snippets-initialize)
;;   )
(use-package tempel
  :ensure t
  :bind
 (:map tempel-map
        ("TAB" . tempel-next))
  :config
  (setq tempel-path
        "~/.emacs.d/config/tempel/templates"))

;;; check error

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  ;; :custom
  ;; (flycheck-disable-checker '(c/c++-clang))
  :config
  (setq flycheck-global-modes '(not python-mode c++-mode c-mode text-mode outline-mode fundamental-mode org-mode diff-mode shell-mode eshell-mode)
        flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-clang-language-standard "c++17"))

;; (use-package flymake
;;   :ensure t
;;   ;; :hook (after-init . flymake-mode)
;;   :config
;;   (setq flymake-run-in-place nil))

;;; format code

;; (use-package format-all
;;   :ensure t)
(require-package 'format-all)

(use-package apheleia
  :ensure t
  :hook ((typescript-tsx-mode . apheleia-mode)
         (json-mode . apheleia-mode)))

;; format c++ or c
(defun format-this-buffer ()
  "Format this all buffer."
  (interactive "")
  (if (or (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (shell-command (concat "astyle "
                             (buffer-file-name)))))

;;; code hide
;; (add-hook 'prog-mode-hook
;;           'hs-minor-mode)
;; (add-hook 'c-mode-hook
;;           'hs-minor-mode)

;; (add-hook 'c++-mode-hook
;;           'hs-minor-mode)

;; (add-hook 'emacs-lisp-mode-hook
;;           'hs-minor-mode)

;; (add-hook 'python-mode-hook
;;           'hs-minor-mode)

;;; tree sitter
(when *is-linux*
  (use-package tree-sitter
    :ensure t
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook
              #'tree-sitter-hl-mode))
  (use-package tree-sitter-langs
    :ensure t))

;;; Doc
(require-package 'helpful)

(use-package docstr
  :ensure t
  :hook (prog-mode . docstr-mode))

;;; Run code
(require-package 'quickrun)

;; write code
;; (use-package eacl
;;   :ensure t)
(require-package 'eacl)

;; search
(require 'color-rg)

(provide 'init-program)
;;; init-program.el ends heres.
