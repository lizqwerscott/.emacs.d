;;; init-custom.el --- Define customizations.        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/quickdir (list "~/" "~/Downloads/" "~/Documents/" "~/MyProject/" "~/github/" user-emacs-directory)
  "Quick dir list."
  :group 'user
  :type '(list dir))

(defcustom user/show-modeline t
  "Show modeline."
  :group 'user
  :type 'boolean)

(defcustom user/dashboard 'dashboard
  "Show dashboard."
  :group 'user
  :type '(choice (const :tag "dashboard" 'dashboard)
                 (const :tag "scratch" 'scratch)
                 (const :tag "enlight" 'enlight)))

(defcustom user/logo (file-truename
                      (concat user-emacs-directory
                              "logos/gnu_color.xpm"))
  "The Logo."
  :group 'user
  :type 'string)

(defcustom user/day-theme 'modus-operandi-tinted
  "Day theme name."
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "Night theme name."
  :group 'user
  :type 'symbol)

(defcustom user/run-python-command "python"
  "A python command for Some package use python package."
  :group 'user
  :type 'string)

(defcustom user/completion-preview-mode-use nil
  "Is use `completion-preview-mode'."
  :group 'user
  :type 'boolean)

(defcustom user/telegap nil
  "Is use telega."
  :group 'user
  :type 'boolean)

(defcustom user/telega-start nil
  "Is auto start telega."
  :group 'user
  :type 'boolean)

(defcustom user/java-lsp nil
  "Is start java lsp for lsp-bridge."
  :group 'user
  :type 'boolean)

(defcustom user/aider nil
  "Aider support."
  :group 'user
  :type 'boolean)

(defcustom user/lsp-client 'eglot
  "The lsp client."
  :group 'user
  :type '(choice (const :tag "eglot" eglot)
                 (const :tag "lsp-bridge" lsp-bridge)))

(defcustom user/flyoverp nil
  "Enable flyover."
  :group 'user
  :type 'boolean)

(defcustom user/dirvish t
  "Drivish support."
  :group 'user
  :type 'boolean)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;; init-custom.el ends here
