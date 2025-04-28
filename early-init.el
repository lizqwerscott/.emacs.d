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

  (require 'init-package)
  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)
  (require 'init-const)
  (when sys/macp
    (add-to-list 'default-frame-alist '(undecorated-round . t)))
  (require 'init-startup)
  (require 'lazy-load)
  (require 'one-key)
  (require 'init-font))
