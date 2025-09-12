;;; init-elisp.el --- elisp                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun eval-buffer-and-message ()
  "Eval elisp buffer and message finish."
  (interactive)
  (eval-buffer)
  (message "Eval buffer finish!"))

(keymap-sets emacs-lisp-mode-map
  '(("C-c r" . eval-buffer-and-message)))

(keymap-sets (emacs-lisp-mode-map lisp-interaction-mode-map)
  '(("C-c C-p" . ielm)
    ("C-h ?" . helpful-at-point)))

(add-hook 'lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))


(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook 'eros-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
