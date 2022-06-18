;;project key
(define-key project-prefix-map
            "t"
            'find-temp-project)
(fset 'project-command-map project-prefix-map)

(meow-leader-define-key
 '("p" . project-command-map))

;;Another command
(meow-leader-define-key
 '("t" . gts-do-translate)
 '("a" . eaf-open))

(meow-leader-define-key
 '("rn" . lsp-bridge-rename)
 '("d" . lsp-bridge-jump-to-next-diagnostic))

;;sly
(meow-leader-define-key
 '("lf" . sly-load-file)
 '("qf" . sly-quickload)
 '("ef" . sly-eval-defun)
 '("el" . sly-eval-last-expression)
 '("C-j" . sly-next-completion)
 '("rs" . sly-restart-inferior-lisp))

(meow-leader-define-key
 '("W" . paredit-wrap-sexp)
 '("S" . paredit-splice-sexp))

;;window key
(meow-leader-define-key
 '("o" . ace-window)
 '("1" . ace-delete-other-windows)
 '("2" . split-window-below)
 '("3" . split-window-horizontally)
 '("0" . ace-delete-window))

;;consult and file
(meow-leader-define-key
 '("sl" . consult-line)
 '("sg" . consult-grep)
 
 '("gi" . consult-imenu)
 '("gm" . consult-imenu-multi)
 '("gl" . consult-goto-line)
 '("go" . consult-outline)

 '("ff" . consult-find)
 '("fs" . ff-find-other-file)

 '("b" . consult-buffer))

;;org
(meow-leader-define-key
 '("ww" . open-my-org-file)
 '("wa" . org-agenda)
 '("wl" . org-store-link)
 '("wc" . org-capture))

;;tab
(meow-leader-define-key
 '("vn" . sort-tab-select-next-tab)
 '("vp" . sort-tab-select-prev-tab)
 '("vc" . sort-tab-close-current-tab))

;;hide
(meow-leader-define-key
 '("ha" . hs-hide-all)
 '("hs" . hs-show-all)
 '("ht" . hs-toggle-hiding))

(meow-define-keys
    'normal
  '("C-s" . save-buffer)
  '("Q" . save-buffers-kill-emacs)
  '("gd" . lsp-bridge-find-def-other-window)
  '("gr" . lsp-bridge-find-references)
  '("gf" . xref-find-definitions)
  '("C-o" . xref-go-back))

(provide 'keybinding)
