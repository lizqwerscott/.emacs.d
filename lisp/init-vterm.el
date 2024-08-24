(require 'vterm)
(require 'multi-vterm)

(setq multi-vterm-dedicated-window-height-percent 40)

;;;###autoload
(defun multi-vterm-run (run-command)
  "Open dedicated `multi-vterm' window."
  (interactive)
  (call-interactively #'multi-vterm-dedicated-toggle)
  (vterm-send-M-w)
  (vterm-send-string run-command t)
  (vterm-send-return))

(provide 'init-vterm)
