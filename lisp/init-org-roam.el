(setq org-roam-directory "~/Documents/Org/roam/")
(org-roam-db-autosync-mode)

(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

(one-key-create-menu
 "Roam"
 '((("f" . "Find roam node") . org-roam-node-find)
   (("c" . "Capture roam node") . org-roam-capture)
   (("i" . "Insert roam node") . org-roam-node-insert)
   (("u" . "Show roam ui") . org-roam-ui-open)))

(provide 'init-org-roam)
