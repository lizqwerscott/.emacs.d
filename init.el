;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(and (file-readable-p custom-file) (load custom-file))

(require 'lib-elisp-utils)
(require 'lib-utils)

(require 'init-packages)

(defun check-user-lisp-change ()
  "Change user lisp file change."
  (interactive)
  (let ((lisp-files (seq-remove (lambda (f) (string= f ".user-lisp-autoloads.el"))
                                (directory-files user-lisp-directory nil (rx ".el" eos))))
        (change-file))
    (dolist (file-name lisp-files)
      (let ((elc-file (expand-file-name (concat (file-name-base file-name) ".elc")
                                        user-lisp-directory))
            (file (expand-file-name file-name
                                    user-lisp-directory)))
        (if (file-exists-p elc-file)
            (let ((file-mtime (file-attribute-modification-time (file-attributes file)))
                  (elc-mtime (file-attribute-modification-time (file-attributes elc-file))))
              (when (time-less-p elc-mtime file-mtime)
                (push file change-file)))
          (push file change-file))))
    change-file))

(let ((autoload-file (expand-file-name ".user-lisp-autoloads.el"
                                       user-lisp-directory)))
  (if (file-exists-p autoload-file)
      (when (check-user-lisp-change)
        (message "Re compile user-lisp file.")
        (prepare-user-lisp nil nil t))
    (prepare-user-lisp)))

(require 'init-startup)
(require 'lazy-load)
(require 'init-font)

(require 'init-gcmh)
(require 'init-super-save)

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
;;(require 'crefactor)
(require 'init-writer)

(require 'init-language)

(require 'init-rsync)
(require 'init-code-stats)
(setopt axis-db-location
        (expand-file-name "var/axis-data.sqlite"
                          user-emacs-directory))
(add-hooks '(prog-mode text-mode)
           #'axis-mode)

(require 'init-ai)

(when user/elfeedp
  (require 'init-elfeed))

(when user/telegap
  (require 'init-telega))

;;; Programming
(require 'init-project)
(require 'init-git)
(require 'init-citre)
(require 'init-program)
(require 'init-lsp)

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
