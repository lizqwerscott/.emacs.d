;;; Tramp
(require 'tramp)

(setq tramp-verbose 0)
(setq tramp-chunksize 2000)
(setq tramp-use-ssh-controlmaster-options nil)

(add-list-to-list 'recentf-exclude
                  tramp-file-name-regexp)
(setq tramp-auto-save-directory temporary-file-directory
      backup-directory-alist (list (cons tramp-file-name-regexp nil)))

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
