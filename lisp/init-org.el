;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Org base
(require 'org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities nil
      org-hide-emphasis-markers t)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0))
(setq org-enforce-todo-dependencies t)

;;; Org function
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (expand-file-name "config/template/template.docx" user-emacs-directory)))
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

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)
   (gnuplot . t)
   (shell . t)))

;;; UI

;; Make invisible parts of Org elements appear visible
(add-hook 'org-mode-hook 'org-appear-mode)

;; 中文标记隐藏空格
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Make verbatim with highlight text background.
;; (add-to-list 'org-emphasis-alist
;;              '("=" (:background "#fef7ca")))
;; Make deletion(obsolote) text foreground with dark gray.
;; (add-to-list 'org-emphasis-alist
;;            '("+" (:foreground "dark gray"
;;                   :strike-through t)))
;; Make code style around with box.
;; (add-to-list 'org-emphasis-alist
;;              '("~" (:box (:line-width 1
;;                                       :color "grey75"
;;                                       :style released-button))))

(require 'valign)
(setq valign-facy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

(require 'org-fancy-priorities)
(setq org-fancy-priorities-list
      '((?A . "A")
        (?B . "⬆")
        (?C . "⬇")
        (?D . "☕")
        (?1 . "⚡")
        (?2 . "2")
        (?3 . "3")
        (?4 . "☕")
        (?I . "Important")))
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;;; keymap
(keymap-sets org-mode-map
  '(("C-c TAB" . "C-c TAB")
    ("M-P" . org-metaup)
    ("M-N" . org-metadown)
    ("M-H" . org-metaleft)
    ("M-L" . org-metaright)

    ("s-<return>" . org-meta-return)
    ("s-P" . org-metaup)
    ("s-N" . org-metadown)
    ("s-H" . org-metaleft)
    ("s-L" . org-metaright)))

;;; capf
(defun my/org-capf ()
  "Capf for org mode."
  (setq-local completion-at-point-functions
              `(cape-file
                cape-emoji
                ,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev)
                pcomplete-completions-at-point)))

(add-hook 'org-mode-hook #'my/org-capf)

;;; another
(setq consult-notes-file-dir-sources
      '(("Org"             ?o "~/Documents/Org/")))
(setq consult-notes-org-headings-files
      '("~/Documents/Org/idea.org"
        "~/Documents/Org/quote.org"))
(consult-notes-org-headings-mode)
(consult-notes-org-roam-mode)

(require 'init-org-roam)

(require 'init-org-capture)

(require 'init-org-agenda)

(require 'init-org-reveal)

(provide 'init-org)
;;; init-org.el ends here.
