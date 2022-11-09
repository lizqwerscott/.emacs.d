;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes


;;; Commentary:


;;; Code:

(require 'package)

;; (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
;; 			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; (setq package-quickstart t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; (update-load-path)
(defun site-lisp-update ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Update site lisp*"))
        (update-git-file (expand-file-name
                          (concat user-emacs-directory
                                  "scripts/updategit.py")))
        (site-lisp-dir (expand-file-name
                        (concat user-emacs-directory
                                "site-lisp/"))))
    (async-shell-command (concat update-git-file
                                 " "
                                 site-lisp-dir)
                         output-buffer)
    (switch-to-buffer-other-window output-buffer)))

(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil)
  :config
  (use-package quelpa-use-package)
  (quelpa-use-package-activate-advice))

(provide 'init-package)
;;; init-package.el ends here
