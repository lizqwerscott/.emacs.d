
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(setq magit-delta-hide-plus-minus-markers nil)

(add-hook 'magit-mode-hook
          #'(lambda ()
              (magit-wip-mode t)
              (magit-delta-mode t)))

(provide 'init-magit)
