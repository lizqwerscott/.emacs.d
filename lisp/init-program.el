;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; Tree-Sitter
(require 'treesit)
(customize-set-variable 'treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

(add-list-to-list 'major-mode-remap-alist
                  '((sh-mode . bash-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (python-mode . python-ts-mode)
                    (c++-mode . c++-ts-mode)
                    (c-mode . c-ts-mode)
                    (go-mode . go-ts-mode)
                    (csharp-mode . csharp-ts-mode)
                    (conf-toml-mode . toml-ts-mode)
                    (js-json-mode . json-ts-mode)
                    (zig-mode . zig-ts-mode)))

;;; format
(lazy-load-global-keys
 '(("C-c j f" . format-code-buffer))
 "init-format")

;;; project
(add-hook 'after-init-hook 'global-projection-hook-mode)

;;; Xref
(setq xref-show-xrefs-function 'consult-xref)
(setq xref-show-definitions-function 'consult-xref)

;; Use faster search tool
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))

(global-set-keys
 '(("M-g r" . xref-find-references)
   ("M-g d" . xref-find-definitions)
   ("M-g D" . xref-find-definitions-other-window)

   ("C-o" . xref-go-back)))

;;; check
(pcase user/lsp-client
  ('eglot
   ;; flycheck
   (require 'flycheck)
   (setq flycheck-emacs-lisp-load-path 'inherit)
   (global-flycheck-mode)

   ;; flyover
   (when user/flyoverp
     (require 'flyover)
     (add-hook 'flycheck-mode-hook #'flyover-mode)
     (setq flyover-use-theme-colors t
           flyover-checkers '(flycheck)
           flyover-show-at-eol t
           flyover-virtual-line-icon (concat (nerd-icons-faicon "nf-fa-arrow_right_long")
                                             " ")
           flyover-virtual-line-type nil))

   ;; flycheck posframe
   ;; (unless user/flyoverp
   ;;   (require 'flycheck-posframe)
   ;;   (add-hook 'flycheck-mode-hook
   ;;             #'flycheck-posframe-mode))

   (global-set-keys
    '(("C-c j d" . consult-flycheck)))))

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

    ;; (add-hook 'eglot-managed-mode-hook
    ;;           #'eldoc-box-hover-at-point-mode)

    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;;; devdocs
(global-set-keys
 '(("C-h C-d" . devdocs-lookup)))

;;; complile
(setq compilation-scroll-output nil)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(defun get-first-compilation-error ()
  (when (compilation-buffer-p (current-buffer))
    (compilation--ensure-parse (point-min))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (progn
            (compilation-next-error 1)
            (> (point)
               (point-min)))
        (error
         nil)))))

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (with-current-buffer buffer
    (when (eq major-mode 'compilation-mode)
      (if (or (string-match "^.*warning.*" string)
              (get-first-compilation-error)
              (string-match ".*exited abnormally.*" string))
          (progn
            (message "Compilation %s" string)
            (goto-char (point-min))
            (call-interactively #'compilation-next-error))
        (message "Build finished :)")
        (run-with-timer 1 nil
                        (lambda ()
                          (when-let* ((multi-window (> (count-windows) 1))
                                      (live (buffer-live-p buffer))
                                      (window (get-buffer-window buffer t)))
                            (delete-window window))))))))

(setq compilation-finish-functions (list #'ar/compile-autoclose-or-jump-first-error))

(defun not-split-window (orig-fn &rest args)
  "Let ORIG-FN not split window.
ARGS is ORIG-FN args."
  (let ((split-height-threshold nil)
        (split-width-threshold nil))
    (apply orig-fn args)))

(advice-add #'next-error-no-select :around #'not-split-window)
(advice-add #'previous-error-no-select :around #'not-split-window)
(advice-add #'compile-goto-error :around #'not-split-window)

;;; eat
(require 'init-eat)

;;; lisp
(add-hook 'before-save-hook
          #'(lambda ()
              (when (or (equal major-mode 'emacs-lisp-mode)
                        (equal major-mode 'lisp-mode)
                        (equal major-mode 'scheme-mode))
                (call-interactively #'check-parens))))

;;; language
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(when user/java
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(add-hook 'sh-mode-hook #'(lambda () (treesit-parser-create 'bash)))

(require 'init-elisp)
(when user/python
  (require 'init-python))
(when user/haskell
  (require 'init-haskell))
(when user/c++
  (require 'init-c++))
(when user/web
  (require 'init-web))
(when user/common-lisp
  (require 'init-common-lisp))
(when user/scheme
  (require 'init-scheme))
(when user/rust
  (require 'init-rust))
(when user/golang
  (require 'init-go))
(when user/zig
  (require 'init-zig))
(when user/sql
  (require 'init-sql))
(when user/godot
  (require 'init-godot))

(provide 'init-program)
;;; init-program.el ends heres.
