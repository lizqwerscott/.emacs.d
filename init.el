
;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

;; (toggle-debug-on-error)
;; 启动必须加载
(require 'init-const)
(require 'init-mode)
(require 'init-hook)
(require 'init-tramp)
(require 'init-key)

(require 'init-ui)

(require 'init-blink-search)
(require 'init-code-stats)

(require 'init-spell)
(require 'init-auto-revert)
;; (require 'init-copilot)

(require 'init-helpful)
(require 'init-auto-save)
(require 'init-input)

(require 'init-edit)

(require 'init-format)
(require 'init-program)
(require 'init-separedit)
;; (require 'init-eglot)
(require 'init-lsp-bridge)
;; (require 'init-python)
(require 'init-c++)
(require 'init-web)
(require 'init-common-lisp)
(require 'init-rust)
;; (require 'init-sql)
(require 'init-go)

(require 'init-rsync)

;; (require 'init-reader)
;; (require 'init-paper)

;;(require 'crefactor)

;; 启动1s后再开启gc管理
(run-with-idle-timer 0.2 0 (lambda ()
			                 (require 'init-meow)
			                 (require 'init-corfu)
                             (require 'init-minibuffer)
			                 (require 'init-gcmh)
                             (require 'init-org)))

;;; init.el ends here.
