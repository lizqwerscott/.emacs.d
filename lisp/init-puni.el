;;; init-puni.el --- puni package                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun puni-jump-out-pair-and-newline ()
  "Puni jump out pair and newline."
  (interactive)
  (puni-mark-sexp-at-point)
  (forward-char 1)
  (newline-and-indent))

(defun puni-wrap-single-quote (&optional n)
  "Wrap the following S-expression with single quote brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "'" "'"))

(defun puni-wrap-double-quote (&optional n)
  "Wrap the following S-expression with double quote brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "\"" "\""))

(with-eval-after-load 'meow
  (require 'meow-lisp-state)

  (meow-normal-define-key
   '("(" . puni-wrap-round)
   '("s-[" . puni-wrap-square)
   '("M-[" . puni-wrap-square)
   '("M-<" . puni-wrap-angle)
   '("s-<" . puni-wrap-angle)
   '("{" . puni-wrap-curly)
   '("\"" . puni-wrap-double-quote)
   '("'" . puni-wrap-single-quote)
   '(")" . puni-splice)
   '("C-j" . puni-jump-out-pair-and-newline)
   '("N" . meow-lisp-mode)))

(keymap-binds puni-mode-map
  ("C-<backword>" . puni-backward-kill-word)

  ("C-s-f" . puni-forward-sexp)
  ("C-s-b" . puni-backward-sexp))

(add-hooks '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook yaml-ts-mode-hook)
           #'puni-mode)

(provide 'init-puni)
;;; init-puni.el ends here
