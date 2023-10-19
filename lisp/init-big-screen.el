(require 'imenu-list)

(add-to-list 'display-buffer-alist
             '("\\*Ilist\\*"
               (imenu-list-display-buffer)
               (window-parameters . ((no-delete-other-windows . t)))))

(add-to-list 'display-buffer-alist
             '("\\*Ilist\\*"
               (imenu-list-display-buffer)
               (window-parameters . ((no-delete-other-windows . t)
                                     (no-other-window . t)))))

;;;###autoload
(defun open-big-screen-mode ()
  "Start big screen mode."
  (interactive)
  (imenu-list-smart-toggle))

(provide 'init-big-screen)
