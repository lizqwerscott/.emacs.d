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

(provide 'init-c++)
;;; init-c++.el ends here
