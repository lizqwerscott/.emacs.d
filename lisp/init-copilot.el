(require 'copilot)

(setq copilot-max-char 1000000)
(keymap-sets copilot-completion-map
  '(("<tab>" . copilot-accept-completion)
    ("TAB" . copilot-accept-completion)))

(add-to-list 'copilot-disable-predicates
             #'(lambda ()
                 (symbol-value 'meow-normal-mode)))

(add-hooks '(python-ts-mode rust-ts-mode c++-ts-mode web-mode bash-ts-mode go-ts-mode csharp-mode csharp-ts-mode)
           #'copilot-mode)

(provide 'init-copilot)
