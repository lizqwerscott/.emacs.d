;;; init-godot.el --- init godot                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(gdscript-mode))

(with-eval-after-load 'gdscript-mode
  (setf gdscript-use-tab-indents nil))

;;; for gdscript
(define-advice jsonrpc--process-filter (:filter-args (args) fix-newline)
  "gdscript eglot."
  (when (string-match-p "\\` \\*EGLOT (.+/.*gdscript-.+) output\\*\\'"
                        (buffer-name (process-buffer (car args))))
    (setcdr args (list (string-replace "\n\n" "\r\n\r\n" (cadr args)))))
  args)

(provide 'init-godot)
;;; init-godot.el ends here
