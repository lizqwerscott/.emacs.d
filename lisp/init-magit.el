
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(magit-todos-mode)

(setq magit-delta-hide-plus-minus-markers nil)

(add-hook 'magit-mode-hook
          #'magit-delta-mode)

(provide 'init-magit)
