;;; init-input.el --- init input package             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott(require 'cl-lib) <lizqwerscott@gmail.com>
;; Keywords: input

;;; Commentary:

;;; Code:

(require 'posframe)
(defun pyim-probe-meow-normal-mode ()
  "probe meow normal mode"
  (symbol-value 'meow-normal-mode))

(use-package pyim
  :ensure t
  :config
  (setq default-input-method "pyim")
  (setq pyim-cloudim 'baidu)
  (setq pyim-page-tooltip 'posframe)
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  (setq-default pyim-english-input-switch-functions
                `(
                  ;; pyim-probe-program-mode
                  pyim-probe-meow-normal-mode
                  pyim-probe-org-structure-template)))

(use-package pyim-basedict
  :ensure t
  :config
  ;; (pyim-basedict-enable)
  )

(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)

(require 'insert-translated-name)
(setq insert-translated-name-translate-engine "youdao")

;;(evil-define-key 'insert 'global (kbd "C-c i") #'insert-translated-name-insert)
(meow-define-keys
    'insert
  '("C-c i" . insert-translated-name-insert))

(provide 'init-input)
