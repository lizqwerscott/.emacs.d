;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@163.com>
;; Keywords: processes,

;;; Commentary:


;;; Code:

;; (use-package paredit
;;   :ensure t)

;;evil
;;evil evil-leader evil-paredit evil-matchit evil-collection

;; (setq evil-want-keybinding nil)
;; (use-package evil
;;   :ensure t
;;   :hook (after-init . evil-mode)
;;   :init
;;   (progn
;;     (setq evil-want-integration t)
;;     (setq evil-want-minibuffer nil)))

;; (use-package evil-leader
;;   :ensure t
;;   :config (progn
;; 	    (evil-leader/set-leader "<SPC>")
;; 	    (global-evil-leader-mode)))

;; (use-package evil-paredit
;;   :ensure t
;;   :after evil)

;; (add-hook 'paredit-mode-hook #'(lambda ()
;;                                  (evil-paredit-mode 1)))

;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :init
;;   (evil-collection-init))

;; (use-package evil-matchit
;;   :ensure t
;;   :init
;;   (global-evil-matchit-mode))

(defun awesome-pair-add-hook ()
  (require 'awesome-pair)
  (dolist (hook (list
               ;; 'c-mode-common-hook
               ;; 'c-mode-hook
               ;; 'c++-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'common-lisp-mode-hook
               'maxima-mode-hook
               'sh-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (awesome-pair-mode 1)))))

(use-package awesome-pair
  :init
  (awesome-pair-add-hook)
  :bind
  (:map awesome-pair-mode-map
        ("(" . awesome-pair-open-round)
        ("[" . awesome-pair-open-bracket)
        ("{" . awesome-pair-open-curly)
        (")" . awesome-pair-close-round)
        ("]" . awesome-pair-close-bracket)
        ("}" . awesome-pair-close-curly)
        ("%" . awesome-pair-match-paren)
        ("\"" . awesome-pair-double-quote)
        ("SPC" . awesome-pair-space)
        ("RET" . awesome-pair-newline)
        ("M-(" . awesome-pair-wrap-round)
        ("M-[" . awesome-pair-wrap-bracket)))

;; (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook #'enable-paredit-mode)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(provide 'init-edit)
;;; init-edit.el ends here.
