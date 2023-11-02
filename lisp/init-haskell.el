
(add-hook 'haskell-mode-hook
          #'interactive-haskell-mode)

(add-hook 'haskell-mode-hook
          #'(lambda ()
              (haskell-indentation-mode -1)))

;; (require 'haskell-mode)
;; (custom-set-variables '(haskell-process-type 'stack-ghci))
;; (keymap-set haskell-indentation-mode-map "RET" #'newline-and-indent)

(provide 'init-haskell)
