;;; Tramp
(require 'tramp)

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:racecar:")
                   "direct-async-process" t))

(setq tramp-verbose 0)
(setq tramp-chunksize 2000)
(setq tramp-use-ssh-controlmaster-options nil)

(setq recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG")
      tramp-auto-save-directory temporary-file-directory
      backup-directory-alist (list (cons tramp-file-name-regexp nil)))

(setq tramp-default-method "ssh")

(provide 'init-tramp)
