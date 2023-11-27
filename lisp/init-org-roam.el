(setq org-roam-directory "~/Documents/Org/roam/")
(org-roam-db-autosync-mode)

(one-key-create-menu
 "Roam"
 '((("f" . "Find roam node") . org-roam-node-find)
   (("c" . "Capture roam node") . org-roam-capture)
   (("i" . "Insert roam node") . org-roam-node-insert)
   (("u" . "Show roam ui") . org-roam-ui-open)))

(provide 'init-org-roam)
