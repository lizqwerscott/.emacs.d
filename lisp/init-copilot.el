(require 'copilot)

(setq copilot-max-char 1000000)
(keymap-sets copilot-completion-map
             '(("<tab>" . copilot-accept-completion)
               ("TAB" . copilot-accept-completion)))

(setq copilot-network-proxy
      `(:host ,user/proxy-host :port 20171))

;;;###autoload
(defun +lizqwer/toggle-copilot ()
  "切换copilot"
  (interactive)
  (call-interactively 'copilot-mode))

;;(global-copilot-mode t)

(provide 'init-copilot)
