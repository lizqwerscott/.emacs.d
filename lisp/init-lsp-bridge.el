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
    (use-package acm-termial
      :quelpa (acm-termial :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git")
      :ensure t)))

(setq lsp-bridge-c-lsp-server "ccls")
(setq acm-backend-lsp-candidates-max-number 4000)
(setq acm-enable-citre nil)
(setq acm-enable-tabnine t)
(setq acm-enable-yas nil)
(setq acm-enable-tempel nil)
(setq lsp-bridge-use-wenls-in-org-mode nil)
(setq lsp-bridge-enable-diagnostics t)

;; (setq lsp-bridge-diagnostic-fetch-idle 0.1)
;; (setq lsp-bridge-enable-debug t)
;; (setq lsp-bridge-python-lsp-server "jedi")
;; (setq lsp-bridge-python-lsp-server "pyright-background-analysis")
(defun enable-lsp-bridge()
  (when-let* ((project (project-current))
              (project-root (if (listp (cdr project))
                                (nth 2 project)
                              (cdr project))))
    (setq-local lsp-bridge-user-langserver-dir project-root
                lsp-bridge-user-multiserver-dir project-root))
  (lsp-bridge-mode))

(defun global-enable-lsp-bridge ()
  (interactive)
  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (enable-lsp-bridge)))))

(setq lsp-bridge-default-mode-hooks
      (remove 'rust-mode-hook lsp-bridge-default-mode-hooks))

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

;; keymap

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

(lazy-one-key-create-menu
 "Code"
 (:key "h" :description "Show document" :command lsp-bridge-popup-documentation :filename "init-lsp-bridge")
 (:key "j" :description "Scroll doc up" :command lsp-bridge-popup-documentation-scroll-up :filename "init-lsp-bridge")
 (:key "k" :description "Scroll doc down" :command lsp-bridge-popup-documentation-scroll-down :filename "init-lsp-bridge")
 (:key "e" :description "Toggle sdcv" :command lsp-bridge-toggle-sdcv-helper :filename "init-lsp-bridge")
 ;; (:key "f" :description "Format code" :command apheleia-format-buffer :filename "init-format")
 (:key "d" :description "Lsp Bridge jump to def" :command lsp-bridge-find-def :filename "init-lsp-bridge")
 (:key "D" :description "Lsp Bridge jump to def other window" :command lsp-bridge-find-def-other-window :filename "init-lsp-bridge")
 (:key "b" :description "Lsp Bridge jump back" :command lsp-bridge-find-def-return :filename "init-lsp-bridge")
 (:key "f" :description "Lsp Bridge find reference" :command lsp-bridge-find-references :filename "init-lsp-bridge")
 (:key "r" :description "Lsp Bridge rename" :command lsp-bridge-rename :filename "init-lsp-bridge")
 (:key "i" :description "Lsp Bridge find impl" :command lsp-bridge-find-impl :filename "init-lsp-bridge")
 (:key "I" :description "Lsp Bridge find impl other window" :command lsp-bridge-find-impl-other-window :filename "init-lsp-bridge")
 (:key "a" :description "Lsp Bridge code action" :command lsp-bridge-code-action :filename "init-lsp-bridge")
 (:key "d" :description "Lsp Bridge toggle diagnostics" :command lsp-bridge-toggle-diagnostics :filename "init-lsp-bridge")
 ;; (:key "`" :description "Add Fold code" :command vimish-fold :filename "init-vimish-fold")
 ;; (:key "~" :description "Delete Fold code" :command vimish-fold-delete :filename "init-vimish-fold")
 )

(global-enable-lsp-bridge)

(provide 'init-lsp-bridge)
