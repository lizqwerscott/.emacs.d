(require 'eshell)
(require 'project)

;;; eshell toggle
(defun eshell-toggle--new-buffer (buf-name)
  "Init BUF-NAME."
  (let ((default-directory (project-root (project-current t)))
        (eshell-buffer-name buf-name))
    (with-temp-buffer
      (call-interactively #'eshell))))

;;;###autoload
(defun eshell-project-toggle ()
  "Show eshell at the bottom of current window and cd to current buffer's path.
Use popper manager eshell buffer."
  (interactive)
  (let ((buf-name (project-prefixed-buffer-name "eshell")))
    (if (get-buffer buf-name)
  	    ;; buffer is already created
        (or (-some-> buf-name get-buffer-window delete-window)
           (switch-to-buffer-other-window buf-name))
      ;; buffer is not created, create it
      (eshell-toggle--new-buffer buf-name))))

;;; eshell prompt
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;; eshell completion
(when (and (executable-find "fish")
         (require 'fish-completion nil t))
  (global-fish-completion-mode))

(provide 'init-eshell)
