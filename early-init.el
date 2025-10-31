;;; early-init.el --- early init                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer noninteractive)

(setq native-comp-jit-compilation nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq native-comp-jit-compilation t)
            (setq native-comp-async-query-on-exit t)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
;; 增加IO性能
(setq read-process-output-max (* 1024 1024 10))
(setq gc-cons-threshold most-positive-fixnum)

(setq load-path-filter-function #'load-path-filter-cache-directory-files)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(setq package-enable-at-startup nil)

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lib")))
  ;; (setq toggle-debug-on-error t)

  (require 'init-const)
  (require 'init-custom)

  (when sys/macp
    (add-to-list 'default-frame-alist '(undecorated-round . t)))
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; early-init.el ends here
