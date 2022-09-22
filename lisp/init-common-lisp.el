;;; init-common-lisp.el --- init common-lisp package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
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

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here.
