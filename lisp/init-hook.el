(add-hook 'typescript-tsx-mode-hook
          #'apheleia-mode)

(add-hook 'json-mode-hook
          #'apheleia-mode)

(add-hook 'sly-mode-hook
          #'(lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

(add-hook 'prog-mode-hook
          #'docstr-mode)

(provide 'init-hook)
