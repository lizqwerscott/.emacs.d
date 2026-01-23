;;; init-tramp.el --- tramp                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'tramp)

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024)
      tramp-verbose 2
      tramp-chunksize 2000)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(add-list-to-list 'recentf-exclude
                  tramp-file-name-regexp)
(setq tramp-auto-save-directory temporary-file-directory
      tramp-backup-directory-alist nil)

(defun my/project-remember-advice (fn pr &optional no-write)
  (let* ((remote? (file-remote-p (project-root pr)))
         (no-write (if remote? t no-write)))
    (funcall fn pr no-write)))

(advice-add 'project-remember-project :around
            'my/project-remember-advice)

(if sys/macp
    (setq tramp-default-method "sshx")
  (setq tramp-default-method "ssh"))

(add-list-to-list 'tramp-remote-path
                  '("~/.guix-profile/bin" "~/.guix-profile/sbin" "/run/current-system/profile/bin" "/run/current-system/profile/sbin"))

(provide 'init-tramp)
;;; init-tramp.el ends here
