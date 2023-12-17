(require 'code-stats)
(setq code-stats-token
      (auth-source-pick-first-password :host "codestats.net"))
(add-hook 'prog-mode-hook #'code-stats-mode)

(defun code-stats-sync-proxy (&optional wait)
  (let ((url-proxy-services `(("http" . ,(concat
                                          user/proxy-host
                                          ":20171"))
                              ("https" . ,(concat
                                           user/proxy-host
                                           ":20171")))))
    (if wait
        (code-stats-sync :wait)
      (code-stats-sync))))

(run-with-idle-timer 30 t
                     #'code-stats-sync-proxy)
(add-hook 'kill-emacs-hook (lambda () (code-stats-sync-proxy :wait)))
(provide 'init-code-stats)
