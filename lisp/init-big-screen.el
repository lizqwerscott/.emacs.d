(require 'sideline)
(require 'sideline-blame)
(require 'imenu-list)

(setq sideline-backends-right
      '(sideline-blame))

;;;###autoload
(defun open-big-screen-mode ()
  "Start big screen mode."
  (interactive)
  (sideline-mode)
  (imenu-list-smart-toggle))

(provide 'init-big-screen)
