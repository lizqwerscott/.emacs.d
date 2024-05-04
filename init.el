
;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

;; (toggle-debug-on-error)
;; 启动必须加载
;; Need install packages
(require 'init-packages)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-theme)
(require 'init-mode)
(require 'init-hook)
(require 'init-key)
(require 'init-hydra)
(require 'init-ui)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-embark)
(require 'init-edit)
(require 'init-lsp-bridge)
(require 'init-gcmh)
(require 'init-code-stats)
(require 'init-auto-revert)
(require 'init-helpful)
(require 'init-auto-save)

(defun last-start ()
  (require 'init-meow)
  (require 'init-magit)
  (require 'init-corfu)
  (require 'init-org)
  ;; (require 'init-pangu)
  (require 'init-tramp)
  (require 'init-blink-search)
  (require 'init-spell)
  (require 'init-copilot)
  (require 'init-input)

  (require 'init-auto-insert)
  (require 'init-program)
  (require 'init-elisp)
  (require 'init-separedit)
  ;; (require 'init-eglot)
  (require 'init-python)
  (require 'init-haskell)
  (require 'init-c++)
  (require 'init-web)
  (require 'init-common-lisp)
  (require 'init-rust)
  ;; (require 'init-sql)
  (require 'init-go)

  (require 'init-rsync)
  (require 'init-codegeex)
  (require 'init-hugo)
  (require 'init-elfeed)
  ;; (require 'init-reader)
  ;; (require 'init-paper)
  ;;(require 'crefactor)
  ;; vivaldi support
  (when user/vivaldi-use
    (setq browse-url-browser-function
          #'browse-url-vivaldi))
  (message "start telega")
  (autoload '+lizqwer/toggle-telega "init-telega" nil t)
  (+lizqwer/toggle-telega)
  (message "load finish")
  )

;; 启动1s后再开启gc管理
(run-with-idle-timer 0.2 nil #'last-start)

;;; init.el ends here.
