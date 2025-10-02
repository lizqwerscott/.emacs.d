;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(and (file-readable-p custom-file) (load custom-file))

(require 'lib-elisp-utils)
(require 'lib-utils)

(require 'init-packages)

(require 'init-startup)
(require 'lazy-load)
(require 'init-font)

(require 'init-gcmh)
(require 'init-super-save)

(require 'init-hydra)
(require 'init-transient)
(require 'init-meow)

(require 'init-ui)

(require 'init-edit)
(require 'init-auto-insert)
(require 'init-separedit)

(require 'init-minibuffer)
(require 'init-completion)

(require 'init-snippet)
(require 'init-tramp)

(require 'init-dired)
(require 'init-ibuffer)
(require 'init-helpful)
(require 'init-calendar)
(require 'init-tools)

(require 'init-org)
;; (require 'init-elfeed)
;;(require 'crefactor)
(require 'init-writer)

(require 'init-language)

(require 'init-rsync)
(require 'init-code-stats)

(require 'init-ai)

(when user/telegap
  (require 'init-telega))

;;; Programming
(require 'init-git)
(require 'init-lsp)
(require 'init-citre)
(require 'init-program)


;; browse support
(setq browse-url-browser-function
      (if sys/macp
          #'browse-url-default-macosx-browser
        #'browse-url-firefox))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
