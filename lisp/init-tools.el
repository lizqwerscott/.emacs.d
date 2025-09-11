;;; init-tools.el --- tools                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

;;; gif screenshot
(with-eval-after-load 'gif-screencast
  (keymap-sets gif-screencast-mode-map
    '(("<f8>" . gif-screencast-toggle-pause)
      ("<f9>" . gif-screencast-stop))))

;;; proced
(with-eval-after-load 'proced
  (setq proced-enable-color-flag t
        proced-tree-flag t
        proced-auto-update-flag 'visible
        proced-descend t
        proced-filter 'user))

(add-hook 'proced-mode-hook
          #'(lambda ()
              (proced-toggle-auto-update 1)))

(provide 'init-tools)
;;; init-tools.el ends here
