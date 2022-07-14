
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))
(require 'cl-lib)

(defun directory-dirs (path)
  "get the path all directory"
  (when (file-directory-p path)
    (cl-remove-if-not #'file-directory-p
                      (cdr (cdr (directory-files path t))))))

(add-to-list 'load-path
       (expand-file-name (concat user-emacs-directory "lisp")))

(mapcar #'(lambda (file)
              (add-to-list 'load-path
                           file))
          (directory-dirs
           (concat user-emacs-directory
                   "site-lisp/")))

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-const)
(require 'init-melpa)
(require 'init-startup)
(require 'init-tool)
(require 'init-package)
;(require 'flycheck-cmake)

(require 'init-project)
(require 'init-func)
;(require 'fcitx)
;(require 'evil-map)
(require 'keybinding)
(require 'init-org)
(require 'init-ui)
(require 'init-input)
(require 'crefactor)

(when (file-exists-p custom-file)
  (load-file custom-file))

