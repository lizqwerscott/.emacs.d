;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes,

;;; Commentary:


;;; Code:

;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'conf-mode-hook 'electric-pair-local-mode)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(use-package grammatical-edit
  :quelpa (grammatical-edit :fetcher github :repo "manateelazycat/grammatical-edit")
  :ensure t)

(require 'grammatical-edit)
(dolist (hook (list
               ;; 'c-mode-common-hook
               ;; 'c-mode-hook
               ;; 'c++-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'lua-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook))
  (add-hook hook '(lambda () (grammatical-edit-mode 1))))

(define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
(define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
(define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
(define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
(define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
(define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
(define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

(define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
(define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
(define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

(define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
(define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

(define-key grammatical-edit-mode-map (kbd "C-s-<backspace>") 'grammatical-edit-kill-current-line)
(define-key grammatical-edit-mode-map (kbd "M-d") 'grammatical-edit-backward-delete)
(define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
(define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

(define-key grammatical-edit-mode-map (kbd "C-c (") 'grammatical-edit-wrap-round)
(define-key grammatical-edit-mode-map (kbd "C-c )") 'grammatical-edit-unwrap)

(define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
(define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
;; (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)


(define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up)

(provide 'init-edit)
;;; init-edit.el ends here.
