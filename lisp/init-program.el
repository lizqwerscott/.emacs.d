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

;; spell
;; (setq ispell-program-name "hunspell")
;; ;; reset the hunspell so it STOPS querying locale!
;; ;; "en_US" is the key to lookup in `ispell-local-dictionary-alist`
;; (setq ispell-local-dictionary "en_US")
;; ;; two dictionaries "en_US" and "zh_CN" are used. Feel free to remove "zh_CN"
;; ;; If `ispell-local-dictionary-alist' is nil, `ispell-local-dictionary' is passed
;; ;; to hunpsell cli program as dictionary.
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
;; ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
;; ;; If it's nil, Emacs tries to automatically set up the dictionaries.
;; (when (boundp 'ispell-hunspell-dictionary-alist)
;;       (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

;; (use-package wucuo
;;   :ensure t
;;   :hook
;;   ((prog-mode . wucuo-start)
;;    (text-mode . wucuo-start)))

;; (use-package jinx
;;   :ensure t
;;   :hook ((org-mode . jinx-mode))
;;   :bind ([remap ispell-word] . jinx-correct)
;;   :config
;;   (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
;;   (vertico-multiform-mode 1)
;;   (add-to-list 'vertico-multiform-categories
;;                '(jinx grid (vertico-grid-annotate . 25))))

;;; format code

;; (use-package format-all
;;   :ensure t
;;   :diminish t)

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

;;; Doc

;; (use-package docstr
;;   :ensure t
;;   :hook (prog-mode . docstr-mode))

(provide 'init-program)
;;; init-program.el ends heres.
