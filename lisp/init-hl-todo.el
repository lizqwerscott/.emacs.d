(global-hl-todo-mode)
(defun hl-todo-rg (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords."
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
       (list regexp
             (rg-read-files)
             (read-directory-name "Base directory: " nil default-directory t)))))
  (rg regexp files dir))

(defun hl-todo-rg-project (regexp &optional files dir)
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
       (list regexp
             (rg-read-files)
             (project-root (project-current))))))
  (rg regexp files dir))

(provide 'init-hl-todo)
