
;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

;; (toggle-debug-on-error)
;; 启动必须加载
(require 'init-mode)
(require 'init-hook)
(require 'init-gcmh)
(require 'init-auto-save)

(require 'init-ui)

(require 'init-key)
(require 'init-hydra)
(require 'init-transient)
(require 'init-meow)

(require 'init-edit)
(require 'init-spell)
(require 'init-input)
(require 'init-auto-insert)
(require 'init-separedit)
(require 'init-auto-revert)

(require 'init-minibuffer)
(require 'init-corfu)
(unless (or (not user/completion-preview-mode-use) user/tabby user/copilot)
  (require 'init-completion-preview))
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

(when user/copilot
  (require 'init-copilot))
(when user/aider
  (require 'init-aider))
(when user/tabby
  (require 'init-tabby))
(when user/codeium
  (require 'init-codeium))

;; (require 'init-codegeex)

(require 'init-gptel)


;;; Programming
(require 'init-magit)
(require 'init-difftastic)
(require 'init-lsp)
(require 'init-citre)
(require 'init-program)

(when (and user/telega-start (display-graphic-p))
  (message "start telega")
  (autoload '+lizqwer/toggle-telega "init-telega" nil t)
  (+lizqwer/toggle-telega))

;;; init.el ends here.
