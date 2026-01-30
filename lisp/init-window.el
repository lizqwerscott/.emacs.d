;;; init-window.el --- init window                   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; ace window
(require 'ace-window)
(add-to-list 'aw-ignored-buffers "*Ilist*")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(add-hook 'after-init-hook
          #'ace-window-posframe-mode)

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t)))))

(transient-define-prefix window-dispatch ()
  "Window Management."
  :transient-non-suffix 'transient--do-stay
  [["Actions"
    ("TAB" "switch" other-window :transient t)
    ("x" "delete" ace-delete-window :transient t)
    ("X" "delete other" ace-delete-other-windows)
    ("s" "swap" ace-swap-window :transient t)
    ("a" "select" ace-select-window)
    ("m" "maximize" toggle-frame-maximized)
    ("u" "fullscreen" toggle-frame-fullscreen)]
   ["Resize"
    ("h" "←" shrink-window-horizontally :transient t)
    ("j" "↓" enlarge-window :transient t)
    ("k" "↑" shrink-window :transient t)
    ("l" "→" enlarge-window-horizontally :transient t)
    ("n" "balance" balance-windows :transient t)]
   ["Split"
    ("3" "horizontally" split-window-right :transient t)
    ("2" "vertically" split-window-below :transient t)
    ("w 3" "root horizontally" split-root-window-right :transient t)
    ("w 2" "root vertically" split-root-window-below :transient t)
    ("t" "Transpose Window Layout" window-layout-transpose :transient t)
    ("r" "Clockwise Rotate" rotate-windows :transient t)
    ("R" "Counterclockwise Rotate" rotate-windows-back :transient t)]
   ["Zoom"
    ("+" "in" text-scale-increase :transient t)
    ("=" "in" text-scale-increase :transient t)
    ("-" "out" text-scale-decrease :transient t)
    ("0" "reset" (lambda () (interactive) (text-scale-increase 0)) :transient t)]
   ["Misc"
    ("o" "frame font" set-frame-font :transient t)
    ("f" "new frame" make-frame-command :transient t)
    ("d" "delete frame" delete-frame :transient t)
    ("<left>" "winner undo" winner-undo :transient t)
    ("<right>" "winner redo" winner-redo :transient t)
    ("q" "Quit" transient-quit-one)]])

(add-to-list 'aw-dispatch-alist '(?w window-dispatch) t)

(global-bind-keys
 (("s-o" "M-o") . ace-window)
 ("C-c w" . window-dispatch))

;;; winner mode
(winner-mode 1)

;;; window rule
(defun my-window-select-fit-size (window)
  "My Auto fit WINDOW size."
  (select-window window)
  (fit-window-to-buffer window
                        (floor (frame-height) 3)
                        10))

(add-list-to-list
 'display-buffer-alist
 '(;; right side window
   ((or (derived-mode . help-mode) (derived-mode . helpful-mode) (derived-mode . fanyi-mode))
    (display-buffer-in-side-window)
    (dedicated . t)
    (side . right)
    (slot . 0)
    (window-width . 0.4)
    (preserve-size t)
    (window-preserve-size . 0.4)
    (body-function . select-window))
   ;; bottom side window
   ((or "\\*.*e?shell\\*" "^\\*.*vterm[inal]*.*\\*.*$" "*ielm*" "*eat*" (derived-mode . sly-mrepl-mode))
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (dedicated . t)
    (window-height . 0.25))
   ((or "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*vc-.*\\**")
    (display-buffer-in-side-window)
    (dedicated . t)
    (side . bottom)
    (slot . 0)
    (window-parameters . ((mode-line-format . none))))
   ((or "\\*Calendar\\*$")
    (display-buffer-in-side-window)
    (dedicated . t)
    (side . bottom)
    (slot . 0))
   ;; bottom buffer (NOT side window)
   ((or "*Org Agenda*" "\\(\\*Capture\\*\\|CAPTURE-.*\\)" "^CAPTURE-.*\\.org*")
    (display-buffer-reuse-mode-window display-buffer-below-selected))
   ((or (derived-mode . compilation-mode) (derived-mode . cargo-process-mode))
    (display-buffer-reuse-mode-window display-buffer-at-bottom)
    (window-height . 0.3)
    (preserve-size . (t . t))
    (body-function . select-window))
   ((or "\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
        "\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
        "Output\\*$"
        "\\*ert\\*$"

        "\\*Async Shell Command\\*$"
        "\\*Apropos\\*$")
    (display-buffer-reuse-mode-window display-buffer-at-bottom)
    (dedicated . t)
    (window-height . 0.25)
    (body-function . my-window-select-fit-size))))

;;; popper
(require 'popper)
(setq popper-group-function #'popper-group-by-directory
      popper-echo-dispatch-actions t
      popper-display-control nil)

(keymap-binds popper-mode-map
  ("C-h z" . popper-toggle)
  ("C-<tab>" . popper-cycle)
  ("C-M-<tab>" . popper-toggle-type))

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

        "^\\*eat*\\$" eat-mode
        "^\\*.*eshell.*\\*.*$" eshell-mode
        "^\\*.*shell.*\\*.*$"
        "^\\*.*terminal.*\\*.*$"
        "^\\*.*vterm[inal]*.*\\*.*$"
        "^\\*.*ielm.*\\*.*$" ielm-mode

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
;;; init-window.el ends here
