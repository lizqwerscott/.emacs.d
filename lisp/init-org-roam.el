;;; init-org-roam.el --- org roam                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq org-roam-directory (file-truename "~/Documents/Org/roam/"))

(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
         :unnarrowed t)
        ("w" "work" plain
         "\n%?\n* 本周工作总结\n\n* 下周工作计划\n\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-工作计划_%<%Y>_${slug}.org" "#+title: 工作计划 %<%Y>.${title}\n#+filetags: :work:")
         :unnarrowed t)
        ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
         :unnarrowed t)))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-enable))

(global-bind-keys
 ("C-c n n l" . org-roam-buffer-toggle)
 ("C-c n n f" . org-roam-node-find)
 ("C-c n n g" . org-roam-graph)
 ("C-c n n i" . org-roam-node-insert)
 ("C-c n n c" . org-roam-capture)
 ("C-c n n j" . org-roam-dailies-capture-today)

 ("C-c n n u" . org-roam-ui-mode))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
