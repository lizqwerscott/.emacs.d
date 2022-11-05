;;; init-web.el --- init web package                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes

;;; Commentary:


;;; Code:

(setq browse-url-browser-function 'browse-url-chrome)

;;; Json
;; (use-package json-mode
;;   :ensure t
;;   :defer 2)
(require-package 'json-mode)

;;; typescript and react
(use-package typescript-mode
  :ensure t
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode
    "TypeScript TSX")
  (add-to-list 'auto-mode-alist
               '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

(use-package tsi
  :quelpa (tsi :fetcher git :url "https://github.com/orzechowskid/tsi.el.git")
  :ensure t
  :after tree-sitter
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;;; Web Hugo
(use-package ox-hugo
  :ensure t
  :init
  (setq org-hugo-base-dir "~/MyProject/website/saveLife")
  (setq org-hugo-default-section-directory "zh-CN/post")
  :after ox)

(provide 'init-web)
;;; init-web.el ends here.
