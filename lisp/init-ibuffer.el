;;; init-ibuffer.el --- init ibuffer                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

(defun ibuffer-refersh ()
  "Open and refresh ibuffer."
  (interactive)
  (when-let* ((buffer (get-buffer "*Ibuffer*")))
    (kill-buffer buffer))
  (ibuffer))

(global-bind-keys
 ("C-x C-b" . ibuffer-refersh))

(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (unless ibuffer-filter-groups
              (require 'projection-ibuffer)
              (ibuffer-projection-set-filter-groups))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
