
(define-key project-prefix-map
            "t"
            'find-temp-project)
(fset 'project-command-map project-prefix-map)

(evil-leader/set-key
  "sl" 'consult-line
  "sg" 'consult-grep

  "gi" 'consult-imenu
  "gm" 'consult-imenu-multi
  "gl" 'consult-goto-line
  "go" 'consult-outline

  "ff" 'consult-find
  "fs" 'ff-find-other-file

  "b" 'consult-buffer

  "a" 'eaf-open
  "d" 'lsp-bridge-jump-to-next-diagnostic

  "o" 'ace-window

  "1" 'ace-delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-horizontally
  "0" 'ace-delete-window

  "lf" 'sly-load-file
  "qf" 'sly-quickload
  "ef" 'sly-eval-defun
  "el" 'sly-eval-last-expression

  "W" 'paredit-wrap-sexp
  "S" 'paredit-splice-sexp
  "C-j" 'sly-next-completion
;  "p" 'projectile-command-map
  "p" 'project-command-map
  "rs" 'sly-restart-inferior-lisp
  ;; "rn" 'eglot-rename
  "rn" 'lsp-bridge-rename

  "ww" 'open-my-org-file
  "wa" 'org-agenda
  "wl" 'org-store-link
  "wc" 'org-capture

  "vn" 'sort-tab-select-next-tab
s "vp" 'sort-tab-select-prev-tab
  "vc" 'sort-tab-close-current-tab

  "ms" 'vc-msg-show
  "t" 'gts-do-translate
  "ha" 'hs-hide-all
  "hs" 'hs-show-all
  "ht" 'hs-toggle-hiding
  ;"hb" 'hs-show-block
  "c" 'comment-or-uncomment-region)

(evil-define-key 'normal 'global (kbd "C-s") 'save-buffer)
(evil-define-key 'normal 'global (kbd "Q") 'save-buffers-kill-emacs)

(evil-define-key 'normal 'lsp-bridge-mode (kbd "gd") 'lsp-bridge-find-def)
(evil-define-key 'normal 'lsp-bridge-mode (kbd "gr") 'lsp-bridge-find-references)

(global-set-key (kbd "C-SPC") nil)

(provide 'evil-map)
