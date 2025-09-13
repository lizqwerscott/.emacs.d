;;; init-haskell.el --- config haskell package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(haskell-mode))

(add-hook 'haskell-mode-hook
          #'interactive-haskell-mode)

(defun run-haskell-project ()
  "Run haskell project."
  (interactive)
  (setq command
        (compilation-read-command
         (concat "cabal run")))
  (require 'multi-vterm)
  (multi-vterm-run command))

(with-eval-after-load 'haskell-mode

  ;; (custom-set-variables '(haskell-process-type 'stack-ghci))

  (keymap-sets haskell-mode-map
    '(("C-c r" . run-haskell-project)
      ("C-c C-p" . run-haskell))))

(provide 'init-haskell)
;;; init-haskell.el ends here
