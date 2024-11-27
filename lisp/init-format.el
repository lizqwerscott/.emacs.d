(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

(setf (alist-get 'c++-ts-mode apheleia-mode-alist)
      'astyle)

(provide 'init-format)
