;;; init-format.el --- init format                   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

(setf (alist-get 'rust-ts-mode apheleia-mode-alist)
      'rustfmt)

(setf (alist-get 'rustfmt apheleia-formatters)
      '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2024"))

(setf (alist-get 'google-java-format apheleia-formatters)
      '("google-java-format" "--aosp" "-"))

(defun format-code-buffer ()
  "Format now buffer."
  (interactive)
  (save-buffer)
  (call-interactively #'apheleia-format-buffer)
  (revert-buffer t t))

(provide 'init-format)
;;; init-format.el ends here.
