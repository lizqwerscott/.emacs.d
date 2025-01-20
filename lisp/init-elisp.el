
(keymap-set emacs-lisp-mode-map
            "C-c C-p"
            #'ielm)

(keymap-set lisp-interaction-mode-map
            "C-c C-p"
            #'ielm)

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

(provide 'init-elisp)
