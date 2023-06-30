(require 'copilot)

(keymap-set copilot-completion-map "<tab>" #'copilot-accept-completion)
(keymap-set copilot-completion-map "TAB" #'copilot-accept-completion)

(setq copilot-network-proxy
      `(:host ,user/proxy-host :port 20171))

;;;###autoload
(defun +lizqwer/toggle-copilot ()
  "切换copilot"
  (interactive)
  (call-interactively 'copilot-mode))

(provide 'init-copilot)
