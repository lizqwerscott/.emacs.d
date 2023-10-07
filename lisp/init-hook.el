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
            (copilot-mode t)
            ))

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode t)
            (recentf-mode t)
            (+evan/scratch-setup)))

;; Enable `read-only-mode' to ensure that we don't change what we can't read.
(add-hook 'redacted-mode-hook
          (lambda ()
            (read-only-mode
             (if redacted-mode 1 -1))))

(provide 'init-hook)
