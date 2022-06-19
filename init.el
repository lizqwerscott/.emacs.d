
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
;; (let ((gc-cons-threshold most-positive-fixnum))
;;   ;load site-lisp
;; )

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Load path
;; (defun update-load-path (&rest _)
;;   "Update 'load-path'."
;;   (dolist (dir '("site-lisp" "lisp"))
;;     (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'.
;; Don't put large files in `site-lisp' directory, e.g. EAF.
;; Otherwise the startup will be very slow. "
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; (update-load-path)
(defun update-site-lisp ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (generate-new-buffer "*Update site lisp*"))
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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-const)
;(require 'init-elpa)
(require 'init-melpa)
(require 'init-startup)
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

(when (file-exists-p custom-file)
  (load-file custom-file))

