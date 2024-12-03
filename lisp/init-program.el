;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:


;;; Ctags

;; (use-package citre
;;   :ensure t)

;;; Lsp Server
;; (require 'init-eglot)

;;; check error
;; flymake
(add-hook 'after-init-hook
          #'flymake-mode)
(setq flymake-run-in-place nil)

;;; debug
(require 'init-dap)

;;; complile
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-max-output-line-length nil)

(defun ar/compile-autoclose (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (when (with-current-buffer buffer
          (equal major-mode 'compilation-mode))
    (if (and (string-match "finished" string)
           (not (string-match "^.*warning.*" string)))
        (progn
          (message "Build finished :)")
          (run-with-timer 3 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window)))))
      (message "Compilation %s" string))))

(setq compilation-finish-functions (list #'ar/compile-autoclose))

;;; language
(require 'init-elisp)
(require 'init-python)
(require 'init-haskell)
(require 'init-c++)
(require 'init-web)
(require 'init-common-lisp)
(require 'init-scheme)
(require 'init-rust)
(require 'init-sql)
(require 'init-go)


(provide 'init-program)
;;; init-program.el ends heres.
