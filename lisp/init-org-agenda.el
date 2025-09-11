;;; init-org-agenda.el --- org agenda                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-agenda)
(require 'org-archive)
(setq org-archive-location "~/Documents/Org/archive.org::* finish-tasks")
(setq org-refile-targets '(("~/Documents/Org/archive.org" :maxlevel . 1)
                           ("~/Documents/Org/inbox.org" :maxlevel . 1)
                           ("~/Documents/Org/tasks.org" :maxlevel . 4)))
(add-list-to-list 'org-agenda-files
                  '("~/Documents/Org/idea.org"
                    "~/Documents/Org/quote.org"
                    "~/Documents/Org/tasks.org"
                    "~/Documents/Org/archive.org"
                    "~/Documents/Org/inbox.org"))

(defun sodaware/switch-task-on-clock-start (task-state)
  "Change a task to 'IN-PROGRESS' when TASK-STATE is 'TODO'."
  (if (string= task-state "TODO")
      "DOING"
    task-state))

;; Use a function to decide what to change the state to.
(setq org-clock-in-switch-to-state #'sodaware/switch-task-on-clock-start)

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
