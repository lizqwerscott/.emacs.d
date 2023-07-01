(add-hook 'typescript-tsx-mode-hook
          #'apheleia-mode)

(add-hook 'json-mode-hook
          #'apheleia-mode)

(add-hook 'python-ts-mode-hook
          #'(lambda ()
              (setq-local compile-command "pdm run start")))

(add-hook 'sly-mode-hook
          #'(lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (docstr-mode t)
            (goggles-mode t)
            (copilot-mode t)))

(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode t)
            (recentf-mode t)
            (global-color-identifiers-mode)
            (+evan/scratch-setup)))

(provide 'init-hook)
