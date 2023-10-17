(require 'code-stats)
(setq code-stats-token
      (auth-source-pick-first-password :host "codestats.net"))
(add-hook 'prog-mode-hook #'code-stats-mode)
(run-with-idle-timer 30 t #'code-stats-sync)
;; (add-hook 'kill-emacs-hook (lambda () (code-stats-sync :wait)))
(provide 'init-code-stats)
