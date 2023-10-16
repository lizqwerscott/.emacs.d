(require 'org-capture)

(setq org-capture-templates nil)

(add-to-list 'org-capture-templates '("t" "Tasks"))
(add-to-list 'org-capture-templates
             '("tp" "Project Write Task" entry
               (file+olp "~/Documents/Sync/org/tasks.org" "Project")
               "* TODO %^{任务名字}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("tr" "Book Reading Task" entry
               (file+olp "~/Documents/Sync/org/tasks.org" "Reading" "Book")
               "* TODO %^{书名字}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("w" "Web Collections" entry
               (file+headline "~/Documents/Sync/org/index.org" "Web")
               "* %U %:annotation\n\n%:initial\n\n%?"))

(add-to-list 'org-capture-templates
             '("i" "Inbox" entry
               (file "~/Documents/Sync/org/index.org")
               "* %U - %^{heading} %^g\n %?\n"))

(provide 'init-org-capture)
