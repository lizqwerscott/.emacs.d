
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))

;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(add-list-to-list 'major-mode-remap-alist
                  '((sh-mode . bash-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (python-mode . python-ts-mode)
                    (c++-mode . c++-ts-mode)
                    (c-mode . c-ts-mode)
                    (go-mode . go-ts-mode)
                    (csharp-mode . csharp-ts-mode)))

;; (autoload 'yaml-mode "yaml-mode")
;; (autoload 'markdown-mode "markdown-mode")
;; (autoload 'python-mode "init-python")
;; (autoload 'cmake-mode "cmake-mode")
;; (autoload 'c++-mode "init-c++")
;; (autoload 'prog-mode "init-separedit")


;; Tree-sitter
;; (setq treesit-extra-load-path
;;       (list
;;        (file-truename "~/.tree-sitter/bin")))

(require 'treesit)
(setq treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

(provide 'init-mode)
