(lazy-load-global-keys
 '(("C-h f" . helpful-callable)
   ("C-h C-f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h C" . help-command))
 "helpful")

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(add-hooks '(help-mode helpful-mode)
           #'visual-line-mode)

(provide 'init-helpful)
