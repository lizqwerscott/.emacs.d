;;; init-const.el --- some const file                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
;; (defconst *is-mac* (eq system-type 'darwin))
;; (defconst *is-linux* (eq system-type 'gnu/linux))
;; (defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")


(provide 'init-const)
;;; init-const.el ends here.
