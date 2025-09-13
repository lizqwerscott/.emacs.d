;;; init-rust.el --- rust config                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(rust-mode cargo))

(add-hook #'rust-ts-mode-hook
          #'(lambda ()
              (require 'rust-cargo)
              (require 'rust-compile)
              (require 'rust-playpen)
              (require 'rust-rustfmt)))

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
              (keymap-sets rust-ts-mode-map
                '(("C-c r" . cargo-process-run)))
              (setq-local compile-command "cargo build")))

(provide 'init-rust)
;;; init-rust.el ends here.
