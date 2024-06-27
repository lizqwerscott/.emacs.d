
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

(require 'init-mode)
(require 'init-hook)
(require 'init-gcmh)
(require 'init-auto-save)

(require 'init-ui)

(require 'init-key)
(require 'init-hydra)
(require 'init-meow)

(require 'init-edit)
(require 'init-spell)
(require 'init-input)
(require 'init-auto-insert)
(require 'init-separedit)
(require 'init-auto-revert)

(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-snippet)
(require 'init-blink-search)
(require 'init-tramp)

(require 'init-dired)
(require 'init-helpful)

(require 'init-org)
;; (require 'init-pangu)
(require 'init-hugo)
(require 'init-elfeed)
;; (require 'init-reader)
;; (require 'init-paper)
;;(require 'crefactor)

(require 'init-go-translate)

(require 'init-rsync)
(require 'init-code-stats)

(require 'init-copilot)
(require 'init-gptel)
(require 'init-codegeex)

;;; Programming
(require 'init-magit)
(require 'init-lsp-bridge)
(require 'init-citre)
(require 'init-program)

(when (and user/telega-start (display-graphic-p))
  (message "start telega")
  (autoload '+lizqwer/toggle-telega "init-telega" nil t)
  (+lizqwer/toggle-telega))

;;; init.el ends here.
