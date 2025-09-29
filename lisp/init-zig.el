;;; init-zig.el --- init zig package                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(zig-ts-mode))

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))

;;; Projection
(with-eval-after-load 'init-project
  (defvar projection-project-type-zig
    (projection-type
     :name 'zig
     :predicate (defun projection-zig-project-p ()
                  (file-exists-p "build.zig"))
     :build "zig build"
     :run "zig build run"
     :test "zig build test"))

  (add-to-list 'projection-project-types projection-project-type-zig))

(require 'project-x)

(defun setting-zig-compile-command ()
  "Setting zig default `compile-command'."
  (let* ((project-path (project-root-path))
         (command (if project-path
                      "zig build run"
                    (concat "zig run "
                            (file-truename (buffer-file-name))))))
    (setq-local compile-command
                command)))

(add-hook 'zig-ts-mode-hook
          #'setting-zig-compile-command)

(with-eval-after-load 'zig-ts-mode
  (keymap-binds zig-ts-mode-map
    ("C-c r" . project-run-command-with-vterm)))

(provide 'init-zig)
;;; init-zig.el ends here
