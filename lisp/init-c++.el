;;; init-c++.el --- init c++ packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes


;;; Commentary:


;;; Code:

;; (require 'flycheck-cmake)

(use-package cmake-mode
  :ensure t
  :diminish t)

(add-to-list 'auto-mode-alist
             '("\\.h\\'" . c++-mode))

;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(provide 'init-c++)
;;; init-c++.el ends here.
