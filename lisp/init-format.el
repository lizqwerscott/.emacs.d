;; init-format.el -*- lexical-binding: t; -*-

(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

;; (setf (alist-get 'rust-ts-mode apheleia-mode-alist)
;;       'cargo-fmt)

;; (setf (alist-get 'cargo-fmt apheleia-formatters)
;;       '("cargo" "fmt"))

(setf (alist-get 'rust-ts-mode apheleia-mode-alist)
      'rustfmt)

(setf (alist-get 'rustfmt apheleia-formatters)
      '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2024"))

(defun format-code-buffer ()
  "Format now buffer."
  (interactive)
  (call-interactively #'save-buffer)
  (call-interactively #'apheleia-format-buffer)
  (call-interactively #'revert-buffer))

(provide 'init-format)
;;; init-format.el ends here.
