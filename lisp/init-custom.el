;;; init-custom.el --- Define customizations.        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/show-modeline t
  "Show modeline."
  :group 'user
  :type 'boolean)

(defcustom user/dashboard t
  "Show dashboard."
  :group 'user
  :type 'boolean)

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

(defcustom user/start-fullscreen t
  "Is fullscreen in start."
  :group 'user
  :type 'boolean)

(defcustom user/start-transparent nil
  "Is transparent in start."
  :group 'user
  :type 'boolean)

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

(defcustom user/ai-completion nil
  "Use what ai to completion: copilot, minuet."
  :group 'user
  :type '(choice (const :tag "copilot" copilot)
                 (const :tag "minuet" minuet)))

(defcustom user/aider nil
  "Aider support."
  :group 'user
  :type 'boolean)

(defcustom user/font-mac-size 230
  "The font size in mac."
  :group 'user
  :type 'number)

(defcustom user/font-win-size 110
  "The font size in windows."
  :group 'user
  :type 'number)

(defcustom user/font-linux-size 190
  "The font size in linux."
  :group 'user
  :type 'number)

(defcustom user/ligature nil
  "Is use ligature."
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
