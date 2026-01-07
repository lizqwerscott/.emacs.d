;;; init-tools.el --- tools                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; which-key

(setq which-key-max-description-length 30
      which-key-lighter nil
      which-key-show-remaining-keys t
      which-key-sort-order 'which-key-description-order)

(which-key-mode)

(with-eval-after-load 'eww
  (keymap-binds eww-mode-map
    ("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)))

;;; gif screenshot
(with-eval-after-load 'gif-screencast
  (keymap-binds gif-screencast-mode-map
    ("<f8>" . gif-screencast-toggle-pause)
    ("<f9>" . gif-screencast-stop)))

;;; proced
(setq proced-auto-update-flag 'visible
      proced-auto-update-interval 3
      proced-enable-color-flag t)

;;; fuzzy clock
(require 'fuzzy-clock-zh)
(setq fuzzy-clock-zh-fuzziness 'fifteen-minutes)
(fuzzy-clock-zh-mode 1)

;;; detached

(require 'detached)

(detached-init)

(connection-local-set-profile-variables
 'remote-detached
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "~/.detached/sessions")))

(connection-local-set-profiles '(:application tramp :protocol "ssh") 'remote-detached)

(provide 'init-tools)
;;; init-tools.el ends here
