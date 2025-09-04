(setq org-roam-directory (file-truename "~/Documents/Org/roam/"))
(org-roam-db-autosync-mode)

(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

(global-set-keys
 '(("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)

   ("C-c n u" . org-roam-ui-mode)))

(provide 'init-org-roam)
