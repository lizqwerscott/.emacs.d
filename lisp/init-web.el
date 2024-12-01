;;; init-web.el --- init web package                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;; vivaldi support
(setq browse-url-browser-function
      (if user/vivaldi-use
          #'browse-url-vivaldi
        (if sys/macp
            #'browse-url-default-macosx-browser
          #'browse-url-firefox)))


(require 'web-mode)
(setq web-mode-indent-style 4)
(setq web-mode-imenu-regexp-list
      (append web-mode-imenu-regexp-list
              '(("\\(function\\) \\(.*\)\\)" 1 2 " > ")
                ("\\(const\\) \\(.*\\)" 1 2 " ")
                ("<\\(.*\\) ?\\(.*\\)>" 1 2 " "))))
;; (pnpm-global-mode)

;;; Json
;; (use-package jsonian
;;   :ensure t
;;   :diminish t
;;   :defer 2)

;;; typescript and react
;; (use-package typescript-mode
;;   :ensure t
;;   :config
;;   (define-derived-mode typescript-tsx-mode typescript-mode
;;     "TypeScript TSX")
;;   (add-to-list 'auto-mode-alist
;;                '("\\.tsx?\\'" . typescript-tsx-mode))
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;                '(typescript-tsx-mode . tsx)))

;; (use-package tsi
;;   :quelpa (tsi :fetcher git :url "https://github.com/orzechowskid/tsi.el.git")
;;   :ensure t
;;   :after tree-sitter
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(provide 'init-web)
;;; init-web.el ends here.
