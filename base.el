(add-to-list 'load-path
             (expand-file-name
              (concat user-emacs-directory "lisp")))

(setq custom-file (locate-user-emacs-file "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-package)
(require 'init-packages)

(defun install-all-packages ()
  (package-refresh-contents)
  (condition-case error-data
      (progn
	    (packages!
	     '(vterm
	       (meow-vterm :fetcher github :repo "accelbread/meow-vterm")))

        (packages!
         (append *package-early-install-list*
                 *package-base-install-list*
                 *package-tool-install-list*
                 *package-language-mode-install-list*
                 *package-edit-install-list*
                 *package-program-install-list*
                 *package-ui-install-list*
                 *package-window-install-list*
                 *package-language-install-list*
                 *package-org-install-list*
                 *package-ai-install-list*
                 *package-rust-install-list*
                 *package-common-lisp-install-list*
                 *package-web-install-list*
                 *package-python-install-list*
                 *package-unity-install-list*
                 *package-sql-install-list*
                 *package-another-install-list*))

        (print (format "Install all packages!"))

        (package-recompile-all))
    (error
     (print (format "Install error: %s" error-data)))))
