
(keymap-set emacs-lisp-mode-map
            "C-c C-p"
            #'ielm)

(keymap-set lisp-interaction-mode-map
            "C-c C-p"
            #'ielm)

;;; nameless
(setq nameless-private-prefix t)
(add-hook 'emacs-lisp-mode-hook #'nameless-mode)

(provide 'init-elisp)
