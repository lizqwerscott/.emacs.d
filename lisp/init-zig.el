;;; init-zig.el --- init zig package                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun run-zig-project ()
  "Run zig project."
  (interactive)
  (autoload 'project-root-path "init-project" nil t)
  (let* ((project-path (project-root-path))
         (command (if project-path
                      "zig build run"
                    (concat "zig run "
                            (file-truename (buffer-file-name))))))
    (setq command (compilation-read-command command))
    (require 'multi-vterm)
    (multi-vterm-run command)))

(with-eval-after-load 'zig-ts-mode
  (keymap-sets zig-ts-mode-map
    '(("C-c r" . run-zig-project))))

(provide 'init-zig)
;;; init-zig.el ends here
