(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

(setf (alist-get 'c++-ts-mode apheleia-mode-alist)
      'astyle)

;;;###autoload
(defun format-code ()
  "Format code."
  (interactive)
  (if (equal major-mode 'rust-ts-mode)
      (cargo-process-fmt)
    (call-interactively 'apheleia-format-buffer)))

(provide 'init-format)
