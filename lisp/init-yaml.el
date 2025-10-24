;;; init-yaml.el ---  yaml                               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(yaml-pro))

(setopt yaml-ts-mode-indent-offset 4)

(add-hook 'yaml-ts-mode-hook
          #'yaml-pro-ts-mode
          100)

(defvar-keymap my/yaml-pro/tree-repeat-map
  :repeat t
  "n" #'yaml-pro-ts-next-subtree
  "p" #'yaml-pro-ts-prev-subtree
  "u" #'yaml-pro-ts-up-level
  "d" #'yaml-pro-ts-down-level
  "m" #'yaml-pro-ts-mark-subtree
  "k" #'yaml-pro-ts-kill-subtree
  "a" #'yaml-pro-ts-first-sibling
  "e" #'yaml-pro-ts-last-sibling)

(with-eval-after-load 'yaml-pro
  (keymap-unset yaml-pro-mode-map "C-c '")
  (keymap-unset yaml-pro-ts-mode-map "C-c '"))

(provide 'init-yaml)
;;; init-yaml.el ends here
