;;; init-const.el --- some const file                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defcustom user/use-proxy nil
  "The proxy host of all."
  :group 'user
  :type 'boolean)

(defcustom user/proxy-host "127.0.0.1"
  "The proxy host of all."
  :group 'user
  :type 'string)

(defcustom user/proxy-all-port 20170
  "The all proxy port of all"
  :group 'user
  :type 'number)

(defcustom user/proxy-http-port 20171
  "The http proxy port of all"
  :group 'user
  :type 'number)

(defcustom user/proxy-rule-port 20172
  "The rule proxy  port of all"
  :group 'user
  :type 'number)

(defcustom user/show-modeline t
  "Show modeline"
  :group 'user
  :type 'boolean)

(defcustom user/dashboard t
  "Show dashboard"
  :group 'user
  :type 'boolean)

(defcustom user/logo (file-truename
                      (concat user-emacs-directory
                              "logos/gnu_color.xpm"))
  "Show dashboard"
  :group 'user
  :type 'string)

(defcustom user/day-theme 'ef-spring
  "User day theme"
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "User night theme"
  :group 'user
  :type 'symbol)

(defcustom user/start-fullscreen t
  "Is fullscreen in start"
  :group 'user
  :type 'boolean)

(defcustom user/start-transparent nil
  "Is transparent in start"
  :group 'user
  :type 'boolean)

(defcustom user/run-python-command "python"
  "A python command for Some package use python package"
  :group 'user
  :type 'string)

(defcustom user/completion-preview-mode-use nil
  "Is use `completion-preview-mode'"
  :group 'user
  :type 'boolean)

(defcustom user/vivaldi-use nil
  "Is use vivaldi"
  :group 'user
  :type 'boolean)

(defcustom user/telega-start nil
  "Is start telega"
  :group 'user
  :type 'boolean)

(defcustom user/java-lsp nil
  "Is start java lsp"
  :group 'user
  :type 'boolean)

(defcustom user/ai-completion nil
  "use what ai to completion: copilot, minuet"
  :group 'user
  :type '(choice (const :tag "copilot" copilot)
                 (const :tag "minuet" minuet)))

(defcustom user/aider nil
  "Is start aider ai"
  :group 'user
  :type 'boolean)

;;;###autoload
(defun get-socks-proxy ()
  (concat "socks5://"
          user/proxy-host
          ":"
          (format "%d" user/proxy-all-port)))

;;;###autoload
(defun get-http-proxy ()
  (concat "http://"
          user/proxy-host
          ":"
          (format "%d" user/proxy-http-port)))

;;;###autoload
(defun get-url-proxy ()
  `(("http" . ,(concat
                user/proxy-host
                ":"
                (format "%d" user/proxy-http-port)))
    ("https" . ,(concat
                 user/proxy-host
                 ":"
                 (format "%d" user/proxy-http-port)))))

(defcustom user/font-mac-size 230
  "The font size in mac"
  :group 'user
  :type 'number)

(defcustom user/font-win-size 110
  "The font size in window"
  :group 'user
  :type 'number)

(defcustom user/font-linux-size 190
  "The font size in linux"
  :group 'user
  :type 'number)

(defcustom user/ligature nil
  "Is use ligature"
  :group 'user
  :type 'boolean)

(defcustom user/lsp-client 'lsp-bridge
  "The lsp client"
  :group 'user
  :type '(choice (const :tag "eglot" eglot)
                 (const :tag "lsp-bridge" lsp-bridge)))

(defcustom user/dirvish t
  "Is Use dirvish"
  :group 'user
  :type 'boolean)

(defcustom user/godot nil
  "Is Use godot"
  :group 'user
  :type 'boolean)

(defcustom user/unity nil
  "Is Use unity"
  :group 'user
  :type 'boolean)

(provide 'init-const)
;;; init-const.el ends here.
