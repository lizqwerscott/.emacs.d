
;;;###autoload
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

(add-hook 'rust-ts-mode-hook
          #'(lambda ()
              (setq-local compile-command "cargo build")))

(provide 'init-rust)
;;; init-rust.el ends here.
