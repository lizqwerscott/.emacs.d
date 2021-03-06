;;project key
(define-key project-prefix-map
            "t"
            'find-temp-project)
(fset 'project-command-map project-prefix-map)

(defun my/meow-quit ()
  (interactive)
  (if (meow-quit)
      (message "finish")))

(defun help-helfup-lsp-bridge ()
  (interactive)
  (helpful-at-point)
  (lsp-bridge-lookup-documentation))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   ;; '("1" . meow-digit-argument)
   ;; '("2" . meow-digit-argument)
   ;; '("3" . meow-digit-argument)
   ;; '("4" . meow-digit-argument)
   ;; '("5" . meow-digit-argument)
   ;; '("6" . meow-digit-argument)
   ;; '("7" . meow-digit-argument)
   ;; '("8" . meow-digit-argument)
   ;; '("9" . meow-digit-argument)
   ;; '("0" . meow-digit-argument)
   ;; '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-leader-define-key
   '("p" . project-command-map))

  ;;Another command
  (meow-leader-define-key
   '("tt" . gts-do-translate)
   '("th" . helpful-at-point)
   ;;'("m" . vc-msg-show)
   )

  ;;lsp bridge
  (meow-leader-define-key
   '("rn" . lsp-bridge-rename)
   '("aa" . lsp-bridge-list-diagnostics)
   '("ah" . lsp-bridge-lookup-documentation)
   )

  ;;sly
  (meow-leader-define-key
   '("lf" . sly-load-file)
   '("qf" . sly-quickload)
   '("ef" . sly-eval-defun)
   '("el" . sly-eval-last-expression)
   '("C-j" . sly-next-completion)
   '("rs" . sly-restart-inferior-lisp))

  (meow-leader-define-key
   ;; '("W" . paredit-wrap-sexp)
   '("W" . awesome-pair-wrap-round)
   ;; '("S" . paredit-splice-sexp)
   '("S" . awesome-pair-unwrap))

  ;;window key
  (meow-leader-define-key
   ;; '("o" . ace-window)
   '("o" . other-window)
   '("1" . ace-delete-other-windows)
   '("2" . split-window-below)
   '("-" . split-window-horizontally)
   '("/" . split-window-vertically)
   '("3" . split-window-horizontally)
   '("0" . ace-delete-window))

  ;;consult and file
  (meow-leader-define-key
   '("sl" . consult-line)
   ;; '("sg" . consult-ripgrep)
   '("si" . consult-imenu)
   '("sm" . consult-imenu-multi)
   '("sg" . consult-goto-line)
   '("so" . consult-outline)
   '("sb" . consult-bookmark)

   '("ff" . find-file)
   '("fr" . consult-find)
   '("F" . affe-find)
   '("fs" . ff-find-other-file)

   '("bb" . consult-buffer)
   '("bB" . consult-buffer-other-window)
   '("bm" . bookmark-set))
  ;;dired
  (meow-leader-define-key
   '("dj" . dired-jump)
   '("dJ" . dired-jump-other-window))
  
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
   '("vc" . sort-tab-close-current-tab)
   '("vm" . sort-tab-close-mode-tabs))

  ;;hide
  (meow-leader-define-key
   '("na" . hs-hide-all)
   '("ns" . hs-show-all)
   '("nt" . hs-toggle-hiding))
  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . comment-or-uncomment-region)
   '("s" . meow-delete)
   '("S" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   ;; '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . my/meow-quit)
   ;;   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill)
   '("D" . meow-kill-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; '("v" . meow-visit)
   '("v" . meow-cancel-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   ;; '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  
  (meow-normal-define-key
    '("C-s" . save-buffer)
    '("Q" . kill-buffer-and-window)
    '("gd" . lsp-bridge-find-def-other-window)
    '("gr" . lsp-bridge-find-references)
    '("gf" . xref-find-definitions)
    '("C-o" . xref-go-back)
    '("/" . consult-ripgrep)
    '("?" . help-helfup-lsp-bridge))
  )

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(provide 'keybinding)
