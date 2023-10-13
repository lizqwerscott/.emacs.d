(require 'lsp-bridge)
(require 'yasnippet)
(require 'common-lisp-snippets)
(yas-global-mode)
(add-hook 'common-lisp-mode-hook
          #'common-lisp-snippets-initialize)

(unless (display-graphic-p)
  (with-eval-after-load 'acm
    (use-package popon
      :quelpa (popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
      :ensure t)
    (use-package acm-terminal
      :quelpa (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git")
      :ensure t)))

(setq lsp-bridge-c-lsp-server "ccls")
(setq acm-backend-search-file-words-enable-fuzzy-match t)
(setq acm-backend-lsp-candidates-max-number 4000)
(setq acm-enable-doc t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium nil)
(setq acm-enable-yas nil)
(setq acm-enable-tempel nil)
(setq lsp-bridge-use-wenls-in-org-mode nil)

(setq lsp-bridge-enable-diagnostics t)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-with-tramp t)

;; (setq lsp-bridge-completion-hide-characters
;;       '(":" ";" "[" "]" "{" "}" ", " "\""))

(setq lsp-bridge-find-def-fallback-function #'dumb-jump)

(setq lsp-bridge-enable-completion-in-string t)

;; (setq lsp-bridge-diagnostic-fetch-idle 0.1)
;; (setq lsp-bridge-enable-debug t)
;; (setq lsp-bridge-python-lsp-server "jedi")
;; (setq lsp-bridge-python-lsp-server "pyright-background-analysis")

(require 'xref)
(defun find-definition-with-lsp-bridge ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition))
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (current-word)))
      (funcall #'xref-find-definitions symb)))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun return-find-def ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (require 'dumb-jump)
    (dumb-jump-back))
   (lsp-bridge-mode
    (lsp-bridge-find-def-return))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(defun +lsp-complete ()
  (interactive)
  (or (tempel-complete t)
     (acm-select-next)))

;;; keymap

(add-hook 'lsp-bridge-mode-hook
          '(lambda ()
             (keymap-set acm-mode-map "C-n" #'acm-select-next)
             (keymap-set acm-mode-map "C-p" #'acm-select-prev)
             (keymap-set acm-mode-map "TAB" #'+lsp-complete)))

(one-key-create-menu
 "Diagnostic"
 '((("n" . "Next diagnostic") . lsp-bridge-diagnostic-jump-next)
   (("p" . "Previous diagnostic") . lsp-bridge-diagnostic-jump-prev)
   (("l" . "Show all diagnostic") . lsp-bridge-diagnostic-list)))

(global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
