;; A minimum .emacs config to test Emacs plugins
(show-paren-mode 1)
(eval-when-compile (require 'cl))

;; test elisps download from internet here
;; (setq test-elisp-dir "~/test-elisp/")
;; (unless (file-exists-p (expand-file-name test-elisp-dir))
;;     (make-directory (expand-file-name test-elisp-dir)))

;; (setq load-path
;;       (append
;;         (loop for dir in (directory-files test-elisp-dir)
;;               unless (string-match "^\\." dir)
;;               collecting (expand-file-name (concat test-elisp-dir dir)))
;;         load-path))

;; package repositories
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(package-initialize)

;; ==== put your code below this line!
;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")

;; (require 'yasnippet)
;; (yas-global-mode 1)

(require 'lsp-bridge)
(setq lsp-bridge-c-lsp-server "ccls")
(global-lsp-bridge-mode)
