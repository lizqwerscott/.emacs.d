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

;;; eldoc
(with-eval-after-load 'eldoc
  (when (childframe-workable-p)
    (require 'eldoc-box)
    (setq eldoc-box-lighter nil
          eldoc-box-only-multi-line t
          eldoc-box-clear-with-C-g t)

    (custom-set-faces
     '(eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
     '(eldoc-box-body ((t (:inherit tooltip)))))

    (add-hook 'eglot-managed-mode-hook
              #'eldoc-box-hover-at-point-mode)

    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

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
          (run-with-timer 1 nil
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
