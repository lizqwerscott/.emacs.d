(require 'imenu-list)

;;;###autoload
(defun open-big-screen-mode ()
  "Start big screen mode."
  (interactive)
  (imenu-list-smart-toggle))

(provide 'init-big-screen)
