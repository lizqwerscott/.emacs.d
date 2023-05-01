
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(autoload 'yaml-mode "yaml-mode")
(autoload 'markdown-mode "markdown-mode")
(autoload 'org-mode "init-org")
(autoload 'python-mode "init-python")
(autoload 'cmake-mode "cmake-mode")
(autoload 'c++-mode "init-c++")
(autoload 'prog-mode "init-separedit")
;; (autoload 'common-lisp-mode "init-common-lisp")

(provide 'init-mode)
