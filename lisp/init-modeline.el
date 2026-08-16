;;; init-modeline.el --- init modeline                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'awesome-tray
  (defun my-module-process-info ()
    (format-mode-line mode-line-process))

  (defface my-module-process-face
    '((((background light)) :inherit awesome-tray-blue-face)
      (t (:inherit awesome-tray-red-face)))
    "Process module face."
    :group 'awesome-tray)

  (add-to-list 'awesome-tray-module-alist
               '("process" . (my-module-process-info my-module-process-face)))

  (setq awesome-tray-date-format "%H:%M")
  (setq awesome-tray-active-modules
        `("meow"
          "location"
          "buffer-name"
          "mode-name"
          ,@(unless (my/unsupport-battery-or-charging)
              '("battery"))
          "process"
          "git"
          "date")))

(if (and (display-graphic-p) (not user/show-modeline))
    (awesome-tray-mode)
  ;; fix doom modeline in master after https://github.com/emacs-mirror/emacs/commit/c6c4888ced296b6bda7752066df44d95f591cb06 commit
  ;; doom modeline issue https://github.com/seagle0128/doom-modeline/issues/826
  (add-hook 'find-file-hook #'(lambda () (setq-local deactivate-mark nil)) 100)
  (require 'doom-modeline)
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-file-name-style
        'buffer-name)
  (setq display-time-string-forms
        '(24-hours ":" minutes " "))
  (add-hook 'after-init-hook
            #'doom-modeline-mode)
  (add-hook 'doom-modeline-mode-hook
            #'(lambda ()
                (display-time-mode)
                (when (and doom-modeline-battery
                           (not (my/unsupport-battery-or-charging)))
                  (display-battery-mode)))))

(provide 'init-modeline)
;;; init-modeline.el ends here
