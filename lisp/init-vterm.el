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

;; (add-hook 'meow-normal-mode-hook
;;           #'(lambda ()
;;               (if (equal major-mode #'vterm-mode)
;;                   (vterm-copy-mode))))

;; (add-hook 'meow-insert-mode-hook
;;           #'(lambda ()
;;               (if (equal major-mode #'vterm-mode)
;;                   (vterm-copy-mode -1))))

(provide 'init-vterm)
