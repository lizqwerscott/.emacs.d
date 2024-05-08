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

(provide 'init-const)
;;; init-const.el ends here.
