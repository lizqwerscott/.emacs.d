(require 'lazy-revert)

(setq auto-revert-verbose t ; let us know when it happens
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      ;; Only prompts for confirmation when buffer is unsaved.
      revert-without-query (list "."))

(add-hook 'after-init-hook
          'lazy-revert-mode)

(provide 'init-auto-revert)
