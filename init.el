(require 'cl-lib)

(defun directory-dirs (path)
  "get the path all directory"
  (when (file-directory-p path)
    (cl-remove-if-not #'file-directory-p
                      (cdr (cdr (directory-files path t))))))

(let ((gc-cons-threshold most-positive-fixnum))
  (add-to-list 'load-path
	       (expand-file-name (concat user-emacs-directory "lisp")))
  ;load site-lisp
  (mapcar #'(lambda (file)
              (add-to-list 'load-path
                           file))
          (directory-dirs
           (concat user-emacs-directory
                   "site-lisp/"))))

(defun update-site-lisp ()
  (interactive)
  (let ((output-buffer (generate-new-buffer "*Update site lisp*")))
    (mapcar #'(lambda (dir)
                (let ((default-directory dir))
                  (when (not (cl-search "emacs-application-framework" dir))
                    (async-shell-command "git pull"
                                         output-buffer))))
          (directory-dirs
           (concat user-emacs-directory
                   "site-lisp/")))))

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
(require 'evil-map)
(require 'init-org)
(require 'init-ui)
(require 'init-input)

(when (file-exists-p custom-file)
  (load-file custom-file))

