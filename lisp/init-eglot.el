(require 'eglot)
(add-hook 'rust-mode-hook
          #'eglot-ensure)

(setq eldoc-echo-area-use-multiline-p 3
      eldoc-echo-area-display-truncation-message nil)
(set-face-attribute 'eglot-highlight-symbol-face nil
                    :background "#b3d7ff")
;; (add-to-list 'eglot-server-programs
;;              '((c-mode c++-mode) . ("ccls")))
;; (add-to-list 'eglot-server-programs
;;              '(python-mode . ("jedi-language-server")))
;; (add-to-list 'eglot-server-programs
;;              '(vue-mode . "vls"))
(add-to-list 'eglot-server-programs
             `(rust-mode . ("rust-analyzer"
                            :initializationOptions (:cargo (:features "all")))))

(provide 'init-eglot)
