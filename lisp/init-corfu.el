
(defun +complete ()
  (interactive)
  (or ;; (tempel-complete t)
   (yas-expand)
   (corfu-next)))

(use-package corfu
  :ensure t
  :init
  ;; (global-corfu-mode)
  (setq corfu-auto t
        ;; corfu-quit-no-match 'separator
        corfu-quit-no-match t
        corfu-preview-current t
        corfu-preselect-first t
        ;; corfu-cycle t
        corfu-auto-prefix 2
        corfu-quit-at-boundary t
        corfu-auto-delay 0.0)
  :bind
  (:map corfu-map
        ("TAB" . +complete)
        ([tab] . +complete)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook
  ((rust-mode sly-mrepl-mode scheme-mode sql-mode eshell-mode inferior-python-mode elvish-mode telega-chat-mode) . corfu-mode))

(setq completion-cycle-threshold 3)
;; (setq tab-always-indent 'complete)


(use-package cape
  :ensure t
  :init
  (add-list-to-list 'completion-at-point-functions
                    '(cape-dabbrev
                      cape-file
                      cape-line))
  (add-hook 'telega-chat-mode-hook
            #'(lambda ()
                (add-to-list 'completion-at-point-functions 'cape-emoji))))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(corfu-history-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

(provide 'init-corfu)
