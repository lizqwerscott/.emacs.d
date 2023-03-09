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

(defcustom user/proxy-rule-port 20172
  "The rule proxy  port of all"
  :group 'user
  :type 'number)

(provide 'init-const)
;;; init-const.el ends here.
