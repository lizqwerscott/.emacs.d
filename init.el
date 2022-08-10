
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)
            (setq gc-cons-percentage 0.6)))
(require 'cl-lib)

(defun directory-dirs (path)
  "Get the path all directory."
  (when (file-directory-p path)
    (cl-remove-if-not #'file-directory-p
                      (cdr (cdr (directory-files path t))))))

(add-to-list 'load-path
             (expand-file-name
              (concat user-emacs-directory "lisp")))

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

;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file "~/.config/emacs-custom.el")

;; (setq debug-on-error t)
(require 'init-const)
(require 'init-package)
(require 'init-startup)
(require 'init-tool)
(require 'init-edit)

(require 'init-program)
(require 'init-python)
(require 'init-c++)
(require 'init-web)
(require 'init-common-lisp)

(require 'init-project)
(require 'init-func)
;(require 'evil-map)
(require 'keybinding)

(add-hook 'org-mode-hook
          (lambda ()
            (require 'init-org)))

(require 'init-ui)
(require 'init-input)
(require 'crefactor)

(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here.
