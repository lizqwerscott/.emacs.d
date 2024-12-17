(require 'imenu-list)

(add-to-list 'display-buffer-alist
             '("\\*Ilist\\*"
               (imenu-list-display-buffer)
               (window-parameters . ((no-delete-other-windows . t)
                                     (no-other-window . t)))))

(provide 'init-imenu-list)
