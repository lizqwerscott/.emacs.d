(require 'lsp-bridge)
(require 'yasnippet)
(require 'common-lisp-snippets)
(setq yas-snippet-dirs
      '("~/.emacs.d/config/yasnippet/snippets/"))
(yas-global-mode 1)
(add-hook 'common-lisp-mode-hook
          #'common-lisp-snippets-initialize)

(add-to-list 'lsp-bridge-default-mode-hooks
             'vue-ts-mode-hook)

(add-to-list 'lsp-bridge-single-lang-server-mode-list
             '(csharp-ts-mode . lsp-bridge-csharp-lsp-server))

(add-to-list 'lsp-bridge-default-mode-hooks
             'csharp-ts-mode-hook)

(setq lsp-bridge-default-mode-hooks
      (remove 'telega-chat-mode-hook
              lsp-bridge-default-mode-hooks))

(unless (display-graphic-p)
  (with-eval-after-load 'acm
    (quelpa '(popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git"))
    (quelpa '(acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git"))
    (require 'popon)
    (require 'acm-terminal)
    ;; (use-package popon
    ;;   :quelpa (popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
    ;;   :ensure t)
    ;; (use-package acm-terminal
    ;;   :quelpa (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git")
    ;;   :ensure t)
    ))

;; (setq lsp-bridge-enable-debug t)

(setq acm-backend-lsp-candidate-min-length 3)
(setq acm-backend-elisp-candidate-min-length 3)
(setq acm-backend-search-file-words-candidate-min-length 3)

(setq acm-backend-search-file-words-enable-fuzzy-match t)
(setq acm-backend-lsp-candidates-max-number 4000)

(setq acm-enable-doc t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium t)
(setq acm-enable-yas nil)
(setq acm-enable-tempel nil)

(setq acm-backend-lsp-show-progress t)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-inlay-hint t)
(setq lsp-bridge-python-command user/run-python-command)
;; (setq acm-candidate-match-function 'orderless-flex)
;; (setq lsp-bridge-use-wenls-in-org-mode nil)
;; (setq lsp-bridge-enable-mode-line nil)
;; (setq lsp-bridge-enable-diagnostics t)
;; (setq lsp-bridge-enable-with-tramp nil)

;; (setq lsp-bridge-completion-hide-characters
;;       '(":" ";" "[" "]" "{" "}" ", " "\""))

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
   (acm-select-next)))

;;; keymap

(keymap-sets acm-mode-map
             '(("C-n" . acm-select-next)
               ("C-p" . acm-select-prev)
               ("TAB" . +lsp-complete)
               ("<tab>" . +lsp-complete)))

(one-key-create-menu
 "Diagnostic"
 '((("n" . "Next diagnostic") . lsp-bridge-diagnostic-jump-next)
   (("p" . "Previous diagnostic") . lsp-bridge-diagnostic-jump-prev)
   (("l" . "Show all diagnostic") . lsp-bridge-diagnostic-list)))

(global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
