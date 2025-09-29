(when user/java-lsp
  (require 'lsp-bridge-jdtls)
  (setq lsp-bridge-enable-auto-import t))

(setq lsp-bridge-enable-mode-line nil)

(require 'lsp-bridge)
(add-to-list 'lsp-bridge-single-lang-server-mode-list
             '(csharp-ts-mode . lsp-bridge-csharp-lsp-server))
(add-to-list 'lsp-bridge-default-mode-hooks
             'csharp-ts-mode-hook)

(add-to-list 'lsp-bridge-single-lang-server-mode-list
             '(lua-ts-mode . "sumneko"))
(add-to-list 'lsp-bridge-default-mode-hooks
             'lua-ts-mode-hook)

(setq lsp-bridge-default-mode-hooks
      (remove 'telega-chat-mode-hook
              lsp-bridge-default-mode-hooks))

(when (not (display-graphic-p))
  (with-eval-after-load 'acm
    (require 'popon)
    (require 'acm-terminal)))

;; (setq lsp-bridge-enable-debug t)

(setq acm-backend-lsp-candidate-min-length 0)
(setq acm-backend-elisp-candidate-min-length 3)
(setq acm-backend-search-file-words-candidate-min-length 3)

(setq acm-backend-search-file-words-enable-fuzzy-match t)
(setq acm-backend-lsp-candidates-max-number 4000)

(setq acm-enable-doc t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium nil)
(setq acm-enable-yas nil)
(setq acm-enable-tempel nil)
(setq acm-enable-citre t)

(setq acm-backend-lsp-show-progress t)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-inlay-hint t)
(setq lsp-bridge-python-command user/run-python-command)
;; (setq acm-candidate-match-function 'orderless-flex)
;; (setq lsp-bridge-use-wenls-in-org-mode nil)
;; (setq lsp-bridge-enable-mode-line nil)
;; (setq lsp-bridge-enable-diagnostics t)
(setq lsp-bridge-enable-with-tramp nil)

;; (setq lsp-bridge-completion-hide-characters
;;       '(":" ";" "[" "]" "{" "}" ", " "\""))

(setq acm-enable-capf t)
(add-to-list 'acm-backend-capf-mode-list 'lisp-mode)

(setq lsp-bridge-find-def-fallback-function #'dumb-jump)

(setq lsp-bridge-enable-completion-in-string t)

(defun find-definition-with-lsp-bridge ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun find-definition-with-lsp-bridge-other-window ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition-other-window))
   ((acm-is-elisp-mode-p)
    (require 'dumb-jump)
    (call-interactively #'dumb-jump-go-other-window))
   (lsp-bridge-mode
    (lsp-bridge-find-def-other-window))
   (t
    (require 'dumb-jump)
    (call-interactively #'dumb-jump-go-other-window))))

(defun return-find-def ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-find-def-return))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(defun +lsp-complete ()
  (interactive)
  (or ;; (tempel-complete t)
   (yas-expand)
   (if user/completion-preview-mode-use
       (completion-preview-insert))
   (ai-complete)
   (acm-select-next)))

;;; keymap

(keymap-binds acm-mode-map
  ("C-n" . acm-select-next)
  ("C-p" . acm-select-prev)
  ("TAB" . +lsp-complete)
  ("<tab>" . +lsp-complete))

(global-lsp-bridge-mode)

(keymap-binds lsp-bridge-mode-map
  ("C-c j r" . lsp-bridge-rename)
  ("C-c j R" . lsp-bridge-start-process)
  ("C-c j a" . lsp-bridge-code-action)
  ("C-c j s" . lsp-bridge-workspace-list-symbols)
  ("C-c j p" . lsp-bridge-peek)

  ("M-g r" . lsp-bridge-find-references)
  ("M-g d" . find-definition-with-lsp-bridge)
  ("M-g D" . find-definition-with-lsp-bridge-other-window)
  ("M-g u" . lsp-bridge-find-impl)
  ("M-g U" . lsp-bridge-find-impl-other-window)

  ("C-h ?" . lsp-bridge-popup-documentation)

  ("C-o" . return-find-def))

(provide 'init-lsp-bridge)
