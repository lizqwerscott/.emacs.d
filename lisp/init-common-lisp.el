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
  (setq sly-complete-symbol-function 'sly-simple-completions)
  ;;(setq sly-complete-symbol-function 'sly-flex-completions)
  (setq inferior-lisp-program "ccl"))

;; (add-hook 'lisp-mode
;;           (lambda ()
;;             (local-set-key (kbd "C-c l") #'sly-load-file)
;;             (local-set-key (kbd "C-c q") #'sly-quickload)
;;             (local-set-key (kbd "C-c ed") #'sly-eval-defun)
;;             (local-set-key (kbd "C-c el") #'sly-eval-last-expression)
;;             (local-set-key (kbd "C-c r") #'sly-restart-inferior-lisp)))


(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here.
