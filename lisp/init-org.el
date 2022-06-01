
;; ga org-agenda gc org-capture gs org-store-link

(defun evil-org-insert-item (&optional checkbox)
  "使用evil 在org mode 插入新行"
  (interactive "p")
  (evil-open-below 1)
  (org-insert-item))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-local-set-key 'normal
                                (kbd "t")
                                'evil-org-insert-item)))
;; space w
(defun open-my-org-file ()
  "打开我的org文件."
  (interactive)
  (find-file "~/Documents/Sync/org/index.org"))

(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name))
                           ".docx"))
        (template-file "/home/lizqwer/Documents/Templates/template.docx"))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file))
    (message "Convert finish: %s" docx-file)))

(defun org-insert-html-head ()
  (interactive)
  (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))

(defun org-insert-html-theme-bigblow ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))

(defun org-insert-html-theme-readtheorg ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)))

(require 'org)

(setq org-startup-indented t
      org-src-tab-acts-natively t)

(setq org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-odd-levels-only t)

(setq org-list-demote-modify-bullet
      (quote (("+" . "-")
              ("-" . "+")
              ("*" . "-")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-"))))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list
        '((?A . "❗")
          (?B . "⬆")
          (?C . "⬇")
          (?D . "☕")
          (?1 . "⚡")
          (?2 . "⮬")
          (?3 . "⮮")
          (?4 . "☕")
          (?I . "Important"))))

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(define-key org-mode-map (kbd "C-c TAB") 'org-insert-item)

(setq org-default-notes-file "~/Documents/Sync/org/index.org")

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

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

(provide 'init-org)
