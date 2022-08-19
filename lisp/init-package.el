;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwer@lzb>
;; Keywords: processes


;;; Commentary:


;;; Code:

(require 'package)

;; (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
;; 			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-quickstart t)

(package-initialize)

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(provide 'init-package)
;;; init-package.el ends here
