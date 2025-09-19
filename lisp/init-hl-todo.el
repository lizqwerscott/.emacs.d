;;; init-hl-todo.el --- init hl todo                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq hl-todo-keyword-faces
      '(("HOLD"   . "#d0bf8f")
        ("TODO"   . "#cc9393")
        ("NEXT"   . "#dca3a3")
        ("THEM"   . "#dc8cc3")
        ("PROG"   . "#7cb8bb")
        ("OKAY"   . "#7cb8bb")
        ("DONT"   . "#5f7f5f")
        ("FAIL"   . "#8c5353")
        ("DONE"   . "#afd8af")
        ("NOTE"   . "#d0bf8f")
        ("HACK"   . "#d0bf8f")
        ("TEMP"   . "#d0bf8f")
        ("FIXME"  . "#cc9393")))

(defun my/hl-todo-generate-rg-regexp ()
  "Generate a regexp string for all TODO keywords.
The regexp will match any of the keywords defined in `hl-todo-keyword-faces',
surrounded by word boundaries."
  (concat "\\s("
          (string-join
           (mapcar #'car
                   hl-todo-keyword-faces)
           "|")
          ")\\s"))

(require 'rg)

(defun hl-todo-rg (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords.
This function interactively prompts for file types and directory to search.
REGEXP is the regular expression to search for.
FILES is the file type pattern to limit the search.
DIR is the base directory for the search."
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (my/hl-todo-generate-rg-regexp)))
       (list regexp
             (rg-read-files)
             (read-directory-name "Base directory: " nil default-directory t)))))
  (rg regexp files dir))

(defun hl-todo-rg-project ()
  "Use `rg' to find all TODO or similar keywords in current project."
  (interactive)
  (unless (require 'rg nil t)
    (error "`rg' is not installed"))
  (rg-project (my/hl-todo-generate-rg-regexp) "everything"))

(global-hl-todo-mode)

(provide 'init-hl-todo)
;;; init-hl-todo.el ends here
