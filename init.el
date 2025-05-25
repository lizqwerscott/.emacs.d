
;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

;; (toggle-debug-on-error)
;; 启动必须加载

(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-packages)

(require 'init-mode)
(require 'init-gcmh)
;; (require 'init-auto-save)
(require 'init-super-save)

(require 'init-key)
(require 'init-hydra)
(require 'init-transient)
(require 'init-meow)

(require 'init-ui)

(require 'init-edit)
(require 'init-spell)
(require 'init-input)
(require 'init-auto-insert)
(require 'init-separedit)
(require 'init-auto-revert)

(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-completion-preview)
(unless (or (not user/completion-preview-mode-use) user/ai-completion)
  ;; Enable Completion Preview mode in code buffers
  (add-hook 'prog-mode-hook #'completion-preview-mode)
  (add-hook 'text-mode-hook #'completion-preview-mode))
(require 'init-snippet)
(require 'init-tramp)

(require 'init-dired)
(require 'init-helpful)
(require 'init-calender)

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

(require 'init-ai)

;;; Programming
(require 'init-git)
(require 'init-lsp)
(require 'init-citre)
(require 'init-program)

(when (and user/telega-start (display-graphic-p))
  (message "start telega")
  (autoload '+lizqwer/toggle-telega "init-telega" nil t)
  (+lizqwer/toggle-telega))

;;; init.el ends here.
