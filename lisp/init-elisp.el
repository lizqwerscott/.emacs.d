
(keymap-set emacs-lisp-mode-map
            "C-c C-p"
            #'ielm)

(keymap-set lisp-interaction-mode-map
            "C-c C-p"
            #'ielm)

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

(add-hook 'emacs-lisp-mode-hook
          #'pmx-setup-check-parens)

(provide 'init-elisp)
