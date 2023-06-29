(add-hook 'typescript-tsx-mode-hook
          #'apheleia-mode)

(add-hook 'json-mode-hook
          #'apheleia-mode)

(add-hook 'sly-mode-hook
          #'(lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (docstr-mode t)
            (goggles-mode t)))

(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode t)
            (recentf-mode t)))

(provide 'init-hook)
