
(keymap-set emacs-lisp-mode-map
            "C-c C-p"
            #'ielm)

(keymap-set lisp-interaction-mode-map
            "C-c C-p"
            #'ielm)

(provide 'init-elisp)
