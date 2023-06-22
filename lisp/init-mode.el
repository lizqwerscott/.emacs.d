
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.rust\\" . rust-ts-mode))

(autoload 'yaml-mode "yaml-mode")
(autoload 'markdown-mode "markdown-mode")
(autoload 'org-mode "init-org")
(autoload 'python-mode "init-python")
(autoload 'cmake-mode "cmake-mode")
;; (autoload 'c++-mode "init-c++")
;; (autoload 'prog-mode "init-separedit")


;; Tree-sitter
(setq treesit-extra-load-path
      (list
       (file-truename "~/.tree-sitter/bin")))

(require 'treesit-auto)
(global-treesit-auto-mode)
(setq treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

(provide 'init-mode)
