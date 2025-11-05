;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:


;;; custom var default value
(setq user/show-modeline t
      user/dashboard t
      user/logo (file-truename
                 (concat user-emacs-directory
                         "logos/gnu_color.xpm"))
      user/day-theme 'modus-operandi-tinted
      user/night-theme 'doom-dracula
      user/start-fullscreen t
      user/start-transparent nil
      user/run-python-command "python"
      user/completion-preview-mode-use nil
      user/telega-start nil

      user/java-lsp nil

      user/ai-completion nil
      user/aider nil

      user/font-size 190
      user/ligature nil
      user/lsp-client 'eglot
      user/flyoverp nil
      user/dirvish t
      )

;;; use program language
(add-hook 'after-init-hook
          (lambda ()

            (require 'init-c++)
            (require 'init-python)

            (require 'init-common-lisp)
            ;; (require 'init-scheme)

            ;; (require 'init-web)
            ;; (require 'init-haskell)
            ;; (require 'init-rust)
            ;; (require 'init-go)
            ;; (require 'init-zig)
            ;; (require 'init-sql)
            ;; (require 'init-java)

            ;; (require 'init-godot)

            ;; (require 'init-plantuml)
            ))

(custom-set-variables
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; custom.el ends here
