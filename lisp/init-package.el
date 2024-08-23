;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defun packages! (packages)
  (dolist (package packages)
    (if (listp package)
        (quelpa package)
      (unless (package-installed-p package)
        (package-refresh-contents)
        (package-install package)))))

(packages! '(quelpa
             ;; quelpa-use-package
             ))

(setq quelpa-update-melpa-p nil)
;; (quelpa-use-package-activate-advice)

;; (update-load-path)
(defun site-lisp-update ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Update site lisp*")))
    (async-shell-command
     (concat "cd "
             user-emacs-directory
             " && git submodule foreach git pull")
     output-buffer)
    (switch-to-buffer-other-window output-buffer)))

;; (use-package quelpa
;;   :ensure t
;;   :custom
;;   (quelpa-update-melpa-p nil)
;;   :config
;;   (use-package quelpa-use-package
;;     :ensure t)
;;   (quelpa-use-package-activate-advice))

(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (site-lisp-update)
  (when (version<= "29" emacs-version)
    (package-upgrade-all))
  (quelpa-upgrade-all))

;;; install all package

(defvar *package-early-install-list*
  '(no-littering
    benchmark-init
    exec-path-from-shell

    pretty-mode
    doom-themes
    dracula-theme
    ef-themes

    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")
    (one-key :fetcher github :repo "manateelazycat/one-key")))

(packages! *package-early-install-list*)

(provide 'init-package)
;;; init-package.el ends here
