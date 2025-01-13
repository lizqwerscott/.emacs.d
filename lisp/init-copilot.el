(require 'copilot)

(setq copilot-max-char 1000000)
(keymap-sets copilot-completion-map
             '(("<tab>" . copilot-accept-completion)
               ("TAB" . copilot-accept-completion)))

(when user/use-proxy
  (setq copilot-network-proxy
        `(:host ,user/proxy-host :port 20171)))

(add-to-list 'copilot-disable-predicates
             #'(lambda ()
                 (symbol-value 'meow-normal-mode)))

(add-hooks '(python-ts-mode rust-ts-mode c++-ts-mode web-mode bash-ts-mode go-ts-mode csharp-mode csharp-ts-mode)
           #'(lambda ()
               (copilot-mode)
               (copilot-diagnose)))

(provide 'init-copilot)
