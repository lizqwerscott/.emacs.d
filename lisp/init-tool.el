
(use-package restart-emacs
  :ensure t)

(use-package benchmark-init
  :ensure t :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/activate))

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000))

(use-package separedit
  :ensure t
  :bind
  (:map prog-mode-map
        ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-buffer-creation-hook #'auto-fill-mode))

(use-package focus
  :ensure t)

(provide 'init-tool)
