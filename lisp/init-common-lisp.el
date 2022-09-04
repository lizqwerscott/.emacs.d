;;; init-common-lisp.el --- init common-lisp package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwer@lzb>
;; Keywords: processes

;;; Commentary:


;;; Code:

(use-package sly
  :ensure t
  :hook (sly-mode . (lambda ()
                      (unless (sly-connected-p)
                        (save-excursion (sly)))))
  :config
  ;; (setq sly-complete-symbol-function 'sly-simple-completions)
  (setq sly-complete-symbol-function 'sly-flex-completions)
  (setq inferior-lisp-program "ccl"))

;; (add-hook 'lisp-mode
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-l") #'sly-eval-last-expression)
;;             (local-set-key (kbd "C-c C-r") #'sly-restart-inferior-lisp)))


;; (use-package sly-quicklisp
;;   :ensure t
;;   :after sly)
(require-package 'sly-quicklisp)

;; (use-package sly-asdf
;;   :ensure t
;;   :after sly)
(require-package 'sly-asdf)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here.
