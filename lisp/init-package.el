;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)
(require 'cl-lib)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(custom-set-variables
 '(package-vc-register-as-project nil))

(defun my/vc-git-clone (fn remote directory rev)
  (if (or (not (string-match-p "elpa" directory))
         (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around
            'my/vc-git-clone)

(cl-defun my/package-vc-install (name &key (fetcher 'github) repo url branch backend local-path)
  (unless (package-installed-p name)
    (if local-path
        (package-vc-install-from-checkout local-path
                                          (symbol-name name))
      (package-vc-install (if (equal 'git fetcher)
                              url
                            (format "https://%s%s"
                                    (pcase fetcher
                                      ('github "github.com/")
                                      ('sourcehut "git.sr.ht/~")
                                      ('codeberg "codeberg.org/"))
                                    repo))
                          branch
                          backend
                          name))))

(defun package! (package)
  (if (listp package)
      (apply #'my/package-vc-install
	         package)
    (unless (package-installed-p package)
	  (package-install package))))

(defun packages! (packages)
  (dolist (package packages)
    (package! package)))

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

(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (site-lisp-update)
  (when (version<= "29" emacs-version)
    (package-upgrade-all))
  (package-vc-upgrade-all))

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

(provide 'init-package)
;;; init-package.el ends here
