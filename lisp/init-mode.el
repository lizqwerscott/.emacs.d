(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

(add-list-to-list 'major-mode-remap-alist
                  '((sh-mode . bash-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (python-mode . python-ts-mode)
                    (cmake-mode . cmake-ts-mode)
                    (c++-mode . c++-ts-mode)
                    (c-mode . c-ts-mode)
                    (go-mode . go-ts-mode)
                    (csharp-mode . csharp-ts-mode)
                    (conf-toml-mode . toml-ts-mode)))

(add-hook 'web-mode-hook
          #'(lambda ()
              (let ((file-name (buffer-file-name)))
                (when file-name
                  (treesit-parser-create
                   (pcase (file-name-extension file-name)
                     ("vue" 'vue)
                     ("html" 'html)
                     ("php" 'php)))))))

(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))

;; Tree-sitter
(require 'treesit)
(customize-set-variable 'treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

(provide 'init-mode)
