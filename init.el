(let ((gc-cons-threshold most-positive-fixnum))
  (add-to-list 'load-path
	       (expand-file-name (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name (concat user-emacs-directory "company")))
  (add-to-list 'load-path
               (expand-file-name (concat user-emacs-directory "site-lisp/corfu-english-helper/")))
  )


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-const)
;(require 'init-elpa)
(require 'init-melpa)
(require 'init-startup)
;(require 'company-sql)
(require 'init-package)

;toggle-corfu-english-helper: toggle on english helper, write english on the fly.
;corfu-english-helper-search: popup english helper manually
(require 'corfu-english-helper)


(require 'init-project)
(require 'init-func)
(require 'fcitx)
(require 'evil-map)
(require 'init-org)
(require 'init-ui)
(require 'init-input)

(when (file-exists-p custom-file)
  (load-file custom-file))

