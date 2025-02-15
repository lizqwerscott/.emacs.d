
;;; ace window
(require 'ace-window)
(add-to-list 'aw-ignored-buffers "*Ilist*")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; winner mode
(winner-mode 1)
(keymap-sets winner-mode-map
             '(("C-c H" . winner-undo)
               ("C-c L" . winner-redo)))

;;; shackle
(require 'shackle)
(setq shackle-default-size 0.5)
(setq shackle-default-alignment 'below)
(setq shackle-rules
      '(;; (help-mode :select t :align t :size 0.4)
        ;; ("*Process List*" :select t :align t)
        ("*One-Key*" :select t :align 'below)
        ("*Org Agenda*" :select t :align 'below)
        ;; ("*eshell*" :regexp t :select t :align 'below)
        ;; (inferior-emacs-lisp-mode :select t :align 'below)
        ))
(shackle-mode 1)

;;; popper
(require 'popper)
(setq popper-group-function #'popper-group-by-directory
      popper-echo-dispatch-actions t)

(keymap-sets popper-mode-map
             '(("C-h z" . popper-toggle)
               ("C-<tab>" . popper-cycle)
               ("C-M-<tab>" . popper-toggle-type)))

(setq popper-reference-buffers
      '("\\*Messages\\*$"
        "Output\\*$" "\\*Pp Eval Output\\*$"
        "^\\*eldoc.*\\*$"
        "\\*Compile-Log\\*$"
        "\\*Completions\\*$"
        "\\*Warnings\\*$"
        "\\*Async Shell Command\\*$"
        "\\*Apropos\\*$"
        "\\*Backtrace\\*$"
        "\\*Calendar\\*$"
        "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
        "\\*Kill Ring\\*$"
        "\\*Embark \\(Collect\\|Live\\):.*\\*$"

        bookmark-bmenu-mode
        comint-mode
        compilation-mode
        help-mode helpful-mode
        tabulated-list-mode
        Buffer-menu-mode

        flymake-diagnostics-buffer-mode
        flycheck-error-list-mode flycheck-verify-mode

        gnus-article-mode devdocs-mode
        grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
        youdao-dictionary-mode osx-dictionary-mode fanyi-mode

        "^\\*Process List\\*$" process-menu-mode
        list-environment-mode cargo-process-mode

        "^\\*.*eshell.*\\*.*$" eshell-mode
        "^\\*.*shell.*\\*.*$"
        "^\\*.*terminal.*\\*.*$"
        "^\\*.*vterm[inal]*.*\\*.*$"

        "\\*DAP Templates\\*$" dap-server-log-mode
        "\\*ELP Profiling Restuls\\*" profiler-report-mode
        "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
        "\\*[Wo]*Man.*\\*$"
        "\\*ert\\*$" overseer-buffer-mode
        "\\*gud-debug\\*$"
        "\\*lsp-help\\*$" "\\*lsp session\\*$"
        "\\*quickrun\\*$"
        "\\*tldr\\*$"
        "\\*vc-.*\\**"
        "\\*diff-hl\\**"
        "^\\*macro expansion\\**"

        "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
        "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
        "\\*docker-.+\\*"
        "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
        "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
        rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

(with-no-warnings
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height)

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

(add-hook 'emacs-startup-hook
          #'popper-echo-mode)

(provide 'init-window)
