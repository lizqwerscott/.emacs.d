(require 'copilot)

(setq copilot-max-char 1000000)
(keymap-sets copilot-completion-map
             '(("<tab>" . copilot-accept-completion)
               ("TAB" . copilot-accept-completion)))

(when user/use-proxy
  (setq copilot-network-proxy
        `(:host ,user/proxy-host :port 20171)))

;;;###autoload
(defun +lizqwer/toggle-copilot ()
  "切换copilot"
  (interactive)
  (call-interactively 'copilot-mode))

;; (global-copilot-mode t)

;; (add-hooks '(python-ts-mode rust-ts-mode c++-ts-mode web-mode bash-ts-mode go-ts-mode csharp-mode csharp-ts-mode)
;;            #'(lambda ()
;;                (copilot-mode)
;;                (copilot-diagnose)))

;; (add-hook 'prog-mode-hook
;;           #'(lambda ()
;;               (when (not (or (equal major-mode 'emacs-lisp-mode)
;;                           (equal major-mode 'lisp-mode)
;;                           (equal major-mode 'lisp-interaction-mode)
;;                           (equal major-mode 'sql-mode)))
;;                 (copilot-mode))))

(provide 'init-copilot)
