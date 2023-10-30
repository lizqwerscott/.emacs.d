(add-to-list 'load-path
             "/home/lizqwer/MyProject/emacs-plugin/codegeex-mode/")

(require 'codegeex)
(keymap-set codegeex-completion-map "<tab>" #'codegeex-accept-completion)
(keymap-set codegeex-completion-map "TAB" #'codegeex-accept-completion)


(provide 'init-codegeex)
