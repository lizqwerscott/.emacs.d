;;; init-elisp.el --- elisp                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-sets emacs-lisp-mode-map
  '(("C-c r" . eval-buffer)))

(keymap-sets (emacs-lisp-mode-map lisp-interaction-mode-map)
  '(("C-c C-p" . ielm)))

(add-hook 'lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))


(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook 'eros-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
