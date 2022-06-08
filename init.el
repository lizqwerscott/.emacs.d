(let ((gc-cons-threshold most-positive-fixnum))
  (add-to-list 'load-path
	       (expand-file-name (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name (concat user-emacs-directory "site-lisp/lsp-bridge")))
  (add-to-list 'load-path
               (expand-file-name (concat user-emacs-directory "site-lisp/awesome-tray")))
  )


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-const)
;(require 'init-elpa)
(require 'init-melpa)
(require 'init-startup)
(require 'init-package)

(require 'init-project)
(require 'init-func)
;(require 'fcitx)
(require 'evil-map)
(require 'init-org)
(require 'init-ui)
(require 'init-input)

(when (file-exists-p custom-file)
  (load-file custom-file))

