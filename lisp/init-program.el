;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; outli
(setq outli-allow-indented-headlines t)
(add-hook 'prog-mode-hook
          #'outli-mode)

(with-hook outline-minor-mode
  (meow-normal-define-key
   '("@" . "C-c @")))

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
                    (js-json-mode . json-ts-mode)))

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

(global-bind-keys
 ("M-g r" . xref-find-references)
 ("M-g d" . xref-find-definitions)
 ("M-g D" . xref-find-definitions-other-window)

 ("C-o" . xref-go-back))

;;; check
(pcase user/lsp-client
  ('eglot
   (defun set-diagnostic-check (symbol value)
     "Set SYMBOL to VALUE and update check."
     (when (bound-and-true-p global-flycheck-mode)
       (global-flycheck-mode nil))
     (remove-hook 'prog-mode-hook
                  #'flymake-mode)
     (set-default-toplevel-value symbol value)
     (pcase value
       ('flymake
        (wait-packages! '(flymake-popon))
        ;; Check elisp with `load-path'
        (defun my-elisp-flymake-byte-compile (fn &rest args)
          "Wrapper for `elisp-flymake-byte-compile'."
          (let ((elisp-flymake-byte-compile-load-path
                 (append elisp-flymake-byte-compile-load-path load-path)))
            (apply fn args)))
        (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)

        (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe
              flymake-margin-indicator-position 'right-margin)

        (add-hook 'prog-mode-hook
                  #'flymake-mode)

        ;; flymake popon
        (setq flymake-popon-width 80)

        (defface posframe-border
          `((t (:inherit region)))
          "Face used by the `posframe' border."
          :group 'posframe)

        (custom-set-faces
         '(flymake-popon ((t :inherit default :height 0.85)))
         `(flymake-popon-posframe-border ((t :foreground ,(face-background 'posframe-border nil t)))))

        (add-hook #'flymake-mode-hook
                  #'flymake-popon-mode)

        (global-bind-keys
         ("C-c j d" . consult-flymake)))
       ('flycheck
        (wait-packages! '(flycheck consult-flycheck flycheck-eglot flycheck-package))
        ;; flycheck
        (require 'flycheck)
        (setq flycheck-emacs-lisp-load-path 'inherit)
        (global-flycheck-mode 1)

        (require 'flycheck-eglot)
        (setq-default flycheck-eglot-exclusive nil)
        (global-flycheck-eglot-mode 1)

        (global-bind-keys
         ("C-c j d" . consult-flycheck)))))

   (defcustom user/diagnostic 'flymake
     "Diagnostic."
     :group 'user
     :type '(choice (const :tag "flymake" flymake)
                    (const :tag "flycheck" flycheck))
     :set #'set-diagnostic-check)

   ;; flyover
   (when user/flyoverp
     (require 'flyover)
     (pcase user/diagnostic
       ('flymake
        (add-hook 'flymake-mode-hook #'flyover-mode))
       ('flycheck
        (add-hook 'flycheck-mode-hook #'flyover-mode)))
     (setq flyover-use-theme-colors t
           flyover-checkers `(,user/diagnostic)
           flyover-show-at-eol t
           flyover-virtual-line-icon (concat (nerd-icons-faicon "nf-fa-arrow_right_long")
                                             " ")
           flyover-virtual-line-type nil))))

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
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)

    (when (equal user/lsp-client 'eglot)
      (global-bind-keys
       ("C-h ?" . eldoc-box-help-at-point)))))

;;; devdocs
(global-bind-keys
 ("C-h C-d" . devdocs-lookup))

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

(with-eval-after-load 'compile
  (keymap-binds compilation-mode-map
    ("s-n" . compilation-next-error)
    ("s-p" . compilation-previous-error)))

;;; eat
(require 'init-eat)

;;; vterm

(defun project-run-command-with-vterm ()
  "Run COMMAND in vterm."
  (interactive)
  (let ((command (compilation-read-command compile-command)))
    (require 'multi-vterm)
    (multi-vterm-run command)))

(with-eval-after-load 'vterm
  (keymap-binds vterm-mode-map
    ("C-y" . vterm-yank)))

;;; lisp
(add-hook 'before-save-hook
          #'(lambda ()
              (when (or (equal major-mode 'emacs-lisp-mode)
                        (equal major-mode 'lisp-mode)
                        (equal major-mode 'scheme-mode))
                (call-interactively #'check-parens))))
;;; latex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-newline-function #'reindent-then-newline-and-indent)

(defun +cdlatex-complete ()
  "TAB complete."
  (interactive)
  (or (yas-expand)
      (cdlatex-tab)))

(with-eval-after-load 'cdlatex
  (keymap-binds cdlatex-mode-map
    ("TAB" . +cdlatex-complete)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (prettify-symbols-mode)
            (cdlatex-mode)
            (setq-local corfu-auto nil)))

;;; language
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

(require 'init-elisp)

;; bash
(add-hook 'sh-mode-hook #'(lambda () (treesit-parser-create 'bash)))

;; json
(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(setq json-ts-mode-indent-offset 4)

;; yaml
(require 'init-yaml)

;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setopt markdown-fontify-code-blocks-natively t)

(provide 'init-program)
;;; init-program.el ends heres.
