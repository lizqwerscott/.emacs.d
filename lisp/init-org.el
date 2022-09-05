;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: org

;;; Commentary:

;;; Code:

;; space ww
(defun open-my-org-file ()
  "打开我的org文件."
  (interactive)
  (affe-find "~/Documents/Sync/org/")
  ;; (find-file "~/Documents/Sync/org/index.org")
  )

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
      org-src-tab-acts-natively t
      org-startup-folded t)

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

;; (use-package org-fancy-priorities
;;   :ensure t
;;   :hook
;;   (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list
;;         '((?A . "A")
;;           (?B . "⬆")
;;           (?C . "⬇")
;;           (?D . "☕")
;;           (?1 . "⚡")
;;           (?2 . "2")
;;           (?3 . "3")
;;           (?4 . "☕")
;;           (?I . "Important"))))

;; (use-package org-bullets
;;   :ensure t
;;   :custom
;;   (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;   (org-ellipsis "⤵")
;;   :hook (org-mode . org-bullets-mode))
(setq org-tag-alist
      '(("@work" . ?w)
        ("@home" . ?h)
        ("@laptop" . ?l)))

;; Make verbatim with highlight text background.
(add-to-list 'org-emphasis-alist
           '("=" (:background "#fef7ca")))
;; Make deletion(obsolote) text foreground with dark gray.
(add-to-list 'org-emphasis-alist
           '("+" (:foreground "dark gray"
                  :strike-through t)))
;; Make code style around with box.
(add-to-list 'org-emphasis-alist
           '("~" (:box (:line-width 1
                        :color "grey75"
                        :style released-button))))

(use-package valign
  :ensure t
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; (setq org-hide-emphasis-markers t)

(require 'org-bars)
(add-hook 'org-mode-hook
          #'org-bars-mode)
(add-hook 'org-mode-hook
          #'org-num-mode)

(setq org-bars-stars '(:empty "◉"
                       :invisible "▶"
                       :visible "▼"))

(define-key org-mode-map (kbd "C-c TAB") 'org-insert-item)
(define-key org-mode-map (kbd "M-h") 'org-metaleft)
(define-key org-mode-map (kbd "M-l") 'org-metaright)

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

(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
        (sequence "REPORT(r!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))

(setq org-log-done 'note)

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable))

(provide 'init-org)
