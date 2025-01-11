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

  (packages!
   '(vterm
     (meow-vterm :fetcher github :repo "accelbread/meow-vterm")))

  (packages! *package-early-install-list*)

  (packages! *package-base-install-list*)
  (packages! *package-tool-install-list*)
  (packages! *package-language-mode-install-list*)
  (packages! *package-edit-install-list*)
  (packages! *package-program-install-list*)
  (packages! *package-ui-install-list*)
  (packages! *package-window-install-list*)
  (packages! *package-language-install-list*)
  (packages! *package-org-install-list*)
  (packages! *package-ai-install-list*)
  (packages! *package-rust-install-list*)
  (packages! *package-common-lisp-install-list*)
  (packages! *package-web-install-list*)
  (packages! *package-python-install-list*)
  (packages! *package-unity-install-list*)
  (packages! *package-sql-install-list*)
  (packages! *package-another-install-list*))
