;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Org function
;; space ww
(defun open-my-org-file ()
  "打开我的org文件."
  (interactive)
  (eaf-open "~/Documents/Sync/org/"))

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
   (latex . t)))

(require 'org)

;;; Org base
(setq org-startup-indented t
      org-src-tab-acts-natively t
      org-startup-folded t)

(setq org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities nil
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

(require 'org-fancy-priorities)
(add-hook 'org-mode-hook org-fancy-priorities-mode)
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

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; (use-package org-bullets
;;   :ensure t
;;   :custom
;;   (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;   (org-ellipsis "⤵")
;;   :hook (org-mode . org-bullets-mode))

;; (setq org-tag-alist
;;       '(("@work" . ?w)
;;         ("@home" . ?h)
;;         ("@laptop" . ?l)))

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


(require 'valign)
(setq valign-facy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

(setq org-hide-emphasis-markers t)

;; (require 'org-bars)
;; (add-hook 'org-mode-hook
;;           #'org-bars-mode)
;; (add-hook 'org-mode-hook
;;           #'org-num-mode)

;; (setq org-bars-stars '(:empty "◉"
;;                        :invisible "▶"
;;                        :visible "▼"))

;;; Org key
(define-key org-mode-map (kbd "C-c TAB") 'org-insert-item)
(define-key org-mode-map (kbd "M-h") 'org-metaleft)
(define-key org-mode-map (kbd "M-l") 'org-metaright)

(setq org-default-notes-file "~/Documents/Sync/org/index.org")

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
        (sequence "REPORT(r!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))

(setq org-log-done 'note)

;; (use-package org-download
;;   :ensure t
;;   :hook (dired-mode . org-download-enable))

(require 'init-org-capture)

(provide 'init-org)
