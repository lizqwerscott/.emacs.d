(require 'vterm)
(require 'multi-vterm)

(setq multi-vterm-dedicated-window-height-percent 40)

;;;###autoload
(cl-defun multi-vterm-run (run-command)
  "Open dedicated `multi-vterm' window."
  (interactive)
  (autoload 'project-root-path "init-project" nil t)
  (if (project-root-path)
      (call-interactively #'multi-vterm-project)
    (call-interactively #'multi-vterm-dedicated-toggle))
  (vterm-send-M-w)
  (vterm-send-string run-command t)
  (vterm-send-return))

(provide 'init-vterm)
