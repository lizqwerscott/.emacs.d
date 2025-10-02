;;; init-org-capture.el --- init org capture         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-capture)

(setq org-capture-templates nil)
(push '("i" "我的闪念" entry (file+headline "~/Documents/Org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n")
      org-capture-templates)
(push '("s" "收藏名言" entry (file+headline "~/Documents/Org/quote.org" "名言") "* %U - %^{标题} %^g\n  %?\n")
      org-capture-templates)
(push '("l" "LNKS" entry (file+headline "~/Documents/Org/lnks.org" "链接") "* [[%^{link-url}][%^{link-description}]] %^g\n:PROPERTIES:\n:LINK-CREATE-TIME: %T\n:END:\n  %?\n")
      org-capture-templates)
(push '("t" "任务" entry (file+headline "~/Documents/Org/tasks.org" "任务") "* TODO %^{标题} %^g\nDEADLINE: %^t SCHEDULED: %^t\n  %?\n") org-capture-templates)
(push '("w" "工作任务" entry (file+headline "~/Documents/Org/tasks.org" "工作任务") "* TODO %^{任务名} :work:\nDEADLINE: %^t SCHEDULED: %^t\n  %?\n" ) org-capture-templates)

(provide 'init-org-capture)
;;; init-org-capture.el ends here
