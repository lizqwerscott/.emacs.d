;;; init-minibuffer.el --- About minibuffer config   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;;; Commentary:

;;; Code:

;;; Minibuffer
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind
  (:map vertico-map ("M-TAB" . #'minibuffer-complete)))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t  ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
                                        ;:hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; (setq register-preview-delay 0.5
  ;;       register-preview-function #'consult-register-format)
  ;; (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;;  :preview-key (kbd "M-."))
  ;; (setq consult-narrow-key "<")
  (consult-preview-at-point-mode)
  )

(use-package consult-project-extra
  :ensure t
  :diminish t)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
