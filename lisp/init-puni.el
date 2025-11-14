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

(require 'transient)
(transient-define-prefix puni-sexp-menu ()
  "Puni sexp menu."
  :transient-non-suffix 'transient--do-stay
  ["Actions"
   ["slurp & barf"
    ("s f" "Slurp forward" puni-slurp-forward :transient t)
    ("s b" "Slurp backward" puni-slurp-backward :transient t)
    ("b f" "Barf forward" puni-barf-forward :transient t)
    ("b b" "Barf backward" puni-barf-backward :transient t)]
   ["Other"
    ("r" "Raise" puni-raise :transient t)
    ("c" "Convolute" puni-convolute :transient t)
    ("p" "Split" puni-split :transient t)
    ("t" "Transpose" puni-transpose :transient t)
    ("S" "Squeeze" puni-squeeze :transient t)]
   ["Move"
    ("F" "Forward Sexp" puni-forward-sexp :transient t)
    ("B" "Backward Sexp" puni-backward-sexp :transient t)
    ("a" "Beginning Sexp" puni-beginning-of-sexp :transient t)
    ("e" "End Sexp" puni-end-of-sexp :transient t)]]

  [("q" "Quit" transient-quit-all)])

(with-eval-after-load 'meow
  (meow-normal-define-key
   '("(" . puni-wrap-round)
   '("s-[" . puni-wrap-square)
   '("M-[" . puni-wrap-square)
   '("{" . puni-wrap-curly)
   '("<" . puni-wrap-angle)
   '("\"" . puni-wrap-double-quote)
   '("'" . puni-wrap-single-quote)
   '(")" . puni-splice)
   '("C-j" . puni-jump-out-pair-and-newline)
   '("X" . puni-sexp-menu)))

(with-eval-after-load 'puni-mode
  (keymap-binds puni-mode-map
    ("C-<backword>" . puni-backward-kill-word)))

(add-hooks '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook yaml-ts-mode-hook)
           #'puni-mode)

(provide 'init-puni)
;;; init-puni.el ends here
