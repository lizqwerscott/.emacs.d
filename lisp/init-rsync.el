
(add-to-list 'load-path
             "/home/lizqwer/MyProject/emacs-plugin/rsync-project-mode/")

(require 'rsync-project-mode)

(setq rsync-project-sync-on-save t)

(add-hook 'prog-mode-hook
          'rsync-project-mode)

(provide 'init-rsync)
