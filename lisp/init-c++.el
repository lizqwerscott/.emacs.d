;;; init-c++.el --- c++                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4
      c-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(with-eval-after-load 'cc-mode
  (autoload #'insert-trailing-semi-and-indent "insert-trailing-semi" nil t)
  (keymap-sets (c-mode-map c++-mode-map)
    '((";" . insert-trailing-semi-and-indent))))

(with-eval-after-load 'c-ts-mode
  (autoload #'insert-trailing-semi-and-indent "insert-trailing-semi" nil t)
  (keymap-sets (c-ts-mode-map c++-ts-mode-map)
    '((";" . insert-trailing-semi-and-indent))))

(provide 'init-c++)
;;; init-c++.el ends here
