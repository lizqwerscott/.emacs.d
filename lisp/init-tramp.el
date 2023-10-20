;;; Tramp
(require 'tramp)

(setq tramp-verbose 0)
(setq tramp-chunksize 2000)
(setq tramp-use-ssh-controlmaster-options nil)

(setq recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG")
      tramp-auto-save-directory temporary-file-directory
      backup-directory-alist (list (cons tramp-file-name-regexp nil)))

(defun my/project-remember-advice (fn pr &optional no-write)
  (let* ((remote? (file-remote-p (project-root pr)))
         (no-write (if remote? t no-write)))
    (funcall fn pr no-write)))

(advice-add 'project-remember-project :around
            'my/project-remember-advice)

(setq tramp-default-method "ssh")

(provide 'init-tramp)
