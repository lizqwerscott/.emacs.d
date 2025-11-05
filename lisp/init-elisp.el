;;; init-elisp.el --- elisp                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun eval-buffer-and-message ()
  "Eval elisp buffer and message finish."
  (interactive)
  (eval-buffer)
  (message "Eval buffer finish!"))

(setopt elisp-fontify-semantically t
        elisp-add-help-echo nil)

(when (cl-find 'doom-dracula custom-enabled-themes)
  (custom-set-faces
   '(elisp-function ((t (:foreground "SpringGreen4"))))))

(keymap-binds (emacs-lisp-mode-map lisp-interaction-mode-map)
  ("C-c r" . eval-buffer-and-message)
  ("C-c C-p" . ielm)
  ("C-h ?" . helpful-at-point)
  ("C-c e" . macrostep-expand))

(add-hook 'lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))

(add-hook 'emacs-lisp-mode-hook 'eros-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
