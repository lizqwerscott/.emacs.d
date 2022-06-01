;;; common-lisp-snippets-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "common-lisp-snippets" "common-lisp-snippets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from common-lisp-snippets.el

(autoload 'common-lisp-snippets-initialize "common-lisp-snippets" "\
Initialize Common Lisp snippets, so Yasnippet can see them." nil nil)

(eval-after-load 'yasnippet '(common-lisp-snippets-initialize))

(register-definition-prefixes "common-lisp-snippets" '("common-lisp-snippets-root"))

;;;***

;;;### (autoloads nil nil ("common-lisp-snippets-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; common-lisp-snippets-autoloads.el ends here
