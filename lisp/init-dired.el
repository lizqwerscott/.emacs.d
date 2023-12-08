(setq dired-recursive-deletes 'always)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

(require 'dired)
(require 'dired-x)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*$"))

(add-hook 'dired-mode-hook
          #'(lambda ()
              (nerd-icons-dired-mode)
              (diredfl-mode)
              (dired-omit-mode)))

(require 'dired-subtree)
(keymap-set dired-mode-map "TAB" #'dired-subtree-cycle)
(keymap-set dired-mode-map "f" #'consult-fd-dir)

(provide 'init-dired)
