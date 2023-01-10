;;; init-rust.el --- init packages about rust        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t)

(use-package cargo-mode
  :ensure t)

(use-package cargo
  :ensure t)

(defun rust-new-project (dir name)
  "Create a rust project."
  (interactive (list (read-directory-name "Project path:"
                                          "~/")
                     (read-string "Project name:")))
  (shell-command
   (concat "cd "
           dir
           " && cargo new "
           name))
  (project-switch-project
   (concat dir
           name)))

(provide 'init-rust)
;;; init-rust.el ends here.
