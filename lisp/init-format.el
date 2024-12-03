(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

(setf (alist-get 'c++-ts-mode apheleia-mode-alist)
      'astyle)

(setf (alist-get 'rust-ts-mode apheleia-mode-alist)
      'cargo-fmt)

(setf (alist-get 'cargo-fmt apheleia-formatters)
      '("cargo" "fmt"))

(provide 'init-format)
