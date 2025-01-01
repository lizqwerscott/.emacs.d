(defun my-module-process-info ()
  (format-mode-line mode-line-process))

(defface my-module-process-face
  '((((background light)) :inherit awesome-tray-blue-face)
    (t (:inherit awesome-tray-red-face)))
  "Process module face."
  :group 'awesome-tray)

(require 'awesome-tray)

(add-to-list 'awesome-tray-module-alist
             '("process" . (my-module-process-info my-module-process-face)))

(setq awesome-tray-date-format "%H:%M")
;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "file-path" "buffer-name" "mode-name" "battery" "git" "date")
;;         '("meow" "location" "file-path" "buffer-name" "mode-name" "git" "date")))
(setq awesome-tray-active-modules
      (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
          '("meow" "location" "buffer-name" "mode-name" "battery" "process" "git" "date")
        '("meow" "location" "buffer-name" "mode-name" "process" "git" "date")))
;; (setq awesome-tray-active-modules
;;       (if (string-match-p "Discharging" (shell-command-to-string "acpi"))
;;           '("meow" "location" "breadcrumb" "mode-name" "battery" "date")
;;         '("meow" "location" "breadcrumb" "mode-name" "date")))

(if (and (display-graphic-p) (not user/show-modeline))
    (progn
      (awesome-tray-mode))
  (progn
    (require 'doom-modeline)
    (setq doom-modeline-time-icon nil)
    (setq doom-modeline-buffer-file-name-style
          'auto)
    (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
    (setq display-time-string-forms
          '(24-hours ":" minutes " "))
    (add-hook 'after-init-hook
              #'doom-modeline-mode)
    (add-hook 'doom-modeline-mode-hook
              #'(lambda ()
                  (display-time-mode)
                  (when doom-modeline-battery
                    (display-battery-mode))))))

(provide 'init-modeline)
