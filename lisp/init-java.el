;;; init-java.el --- config java                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(groovy-mode))

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(provide 'init-java)
;;; init-java.el ends here
