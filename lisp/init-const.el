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

(defcustom user/day-theme 'ef-spring
  "User day theme"
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "User night theme"
  :group 'user
  :type 'symbol)

(defcustom user/run-python-command "python"
  "A python command for Some package use python package"
  :group 'user
  :type 'string)

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

(defcustom user/tabby nil
  "Is start tabby ai completion"
  :group 'user
  :type 'boolean)

(defcustom user/codeium nil
  "Is start codeium ai completion"
  :group 'user
  :type 'boolean)

(defcustom user/copilot nil
  "Is start copilot ai completion"
  :group 'user
  :type 'boolean)

(defcustom user/aider nil
  "Is start aider ai"
  :group 'user
  :type 'boolean)

(defvar user/now-theme nil)

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

(defcustom user/lsp-client 'lsp-bridge
  "The lsp client"
  :group 'user
  :type 'symbol)

(provide 'init-const)
;;; init-const.el ends here.
