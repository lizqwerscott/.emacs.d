;;; init-puni.el --- puni package                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun puni-jump-out-pair-and-newline ()
  "Puni jump out pair and newline."
  (interactive)
  (puni-end-of-sexp)
  (forward-char 1)
  (newline-and-indent))

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
   '("{" . puni-wrap-curly)
   '("<" . puni-wrap-angle)
   '(")" . puni-splice)
   '("C-j" . puni-jump-out-pair-and-newline)
   '("X" . puni-sexp-menu)))

(add-hooks '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook yaml-ts-mode-hook)
           #'puni-mode)

(provide 'init-puni)
;;; init-puni.el ends here
