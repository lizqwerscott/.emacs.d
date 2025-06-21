(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
;; 增加IO性能
(setq read-process-output-max (* 1024 1024 10))
(setq gc-cons-threshold most-positive-fixnum)
(find-function-setup-keys)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-vc-git-default-clone-depth 1)

(defun packages! (packages)
  (dolist (package packages)
    (straight-use-package package)))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lib")))
  ;; (setq toggle-debug-on-error t)
  (require 'init-utils)
  (add-subdirs-to-load-path
   (concat user-emacs-directory
           "site-lisp/"))

  (setq custom-file (locate-user-emacs-file "custom.el"))

  (defvar *package-early-install-list*
    '(no-littering
      benchmark-init
      exec-path-from-shell

      pretty-mode
      doom-themes

      (lazy-load :host github :repo "manateelazycat/lazy-load")
      (one-key :host github :repo "manateelazycat/one-key")))

  (packages! *package-early-install-list*)
  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)
  (require 'init-const)
  (when sys/macp
    (add-to-list 'default-frame-alist '(undecorated-round . t)))
  (require 'init-startup)
  (require 'lazy-load)
  (require 'one-key)
  (require 'init-font))
