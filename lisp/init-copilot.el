(require 'copilot)

;; (add-hook 'prog-mode-hook
;;           'copilot-mode)

(keymap-set copilot-completion-map "<tab>" #'copilot-accept-completion)
(keymap-set copilot-completion-map "TAB" #'copilot-accept-completion)

(setq copilot-network-proxy
      '(:host "10.0.96.67" :port 20171))

;;;###autoload
(defun +lizqwer/toggle-copilot ()
  "切换copilot"
  (interactive)
  (call-interactively 'copilot-mode))

(provide 'init-copilot)
