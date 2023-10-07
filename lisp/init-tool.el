;;; init-tool.el --- init tool packages              -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;; (use-package ag
;;   :ensure t
;;   :config
;;   (setq ag-highlight-search t))

;; Nice writing
;; (use-package olivetti
;;   :ensure t
;;   :diminish
;;   :init (setq olivetti-body-width 0.618))

;;; tree sitter
;; (when sys/linuxp
;;   (use-package tree-sitter
;;     :ensure t
;;     :config
;;     (global-tree-sitter-mode)
;;     (add-hook 'tree-sitter-after-on-hook
;;               #'tree-sitter-hl-mode))
;;   (use-package tree-sitter-langs
;;     :ensure t))

(provide 'init-tool)
;;; init-tool.el ends here.
