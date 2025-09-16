;;; init-haskell.el --- config haskell package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(haskell-mode))

(add-hook 'haskell-mode-hook
          #'interactive-haskell-mode)

(defun setting-haskell-compile-command ()
  "Setting haskell default `compile-command'."
  (setq-local compile-command
              "cabal run"))

(add-hook 'haskell-mode-hook
          #'setting-haskell-compile-command)

(with-eval-after-load 'haskell-mode

  ;; (custom-set-variables '(haskell-process-type 'stack-ghci))

  (keymap-sets haskell-mode-map
    '(("C-c r" . project-run-command-with-vterm)
      ("C-c C-p" . run-haskell))))

(provide 'init-haskell)
;;; init-haskell.el ends here
