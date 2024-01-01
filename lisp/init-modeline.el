(setq awesome-tray-date-format "%H:%M")
;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "file-path" "buffer-name" "mode-name" "battery" "git" "date")
;;         '("meow" "location" "file-path" "buffer-name" "mode-name" "git" "date")))
(setq awesome-tray-active-modules
      (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
          '("meow" "location" "buffer-name" "mode-name" "battery" "git" "date")
        '("meow" "location" "buffer-name" "mode-name" "git" "date")))
;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "breadcrumb" "mode-name" "battery" "date")
;;         '("meow" "location" "breadcrumb" "mode-name" "date")))

(if user/show-modeline
    (progn
      (require 'doom-modeline)
      (setq doom-modeline-buffer-file-name-style
            'file-name)
      (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
      (add-hook 'after-init-hook
                #'doom-modeline-mode))
  (when (display-graphic-p)
    (awesome-tray-mode)))

(provide 'init-modeline)
