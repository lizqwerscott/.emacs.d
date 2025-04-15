
;;; ace window
(require 'ace-window)
(add-to-list 'aw-ignored-buffers "*Ilist*")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t)))))

(pretty-hydra-define-e hydra-window
  (:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
          :foreign-keys warn :quit-key ("q" "C-g") :posframe t)
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("X" ace-delete-other-windows "delete other" :exit t)
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select" :exit t)
    ("m" toggle-frame-maximized "maximize" :exit t)
    ("u" toggle-frame-fullscreen "fullscreen" :exit t))
   "Resize"
   (("h" shrink-window-horizontally "←")
    ("j" enlarge-window "↓")
    ("k" shrink-window "↑")
    ("l" enlarge-window-horizontally "→")
    ("n" balance-windows "balance"))
   "Split"
   (("3" split-window-right "horizontally")
    ("2" split-window-below "vertically")
    ("t" rotate-window-layout-clockwise "Clockwise Rotate")
    ("T" rotate-window-layout-counterclockwise "Counterclockwise Rotate"))
   "Zoom"
   (("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-increase 0) "reset"))
   "Misc"
   (("o" set-frame-font "frame font")
    ("f" make-frame-command "new frame")
    ("d" delete-frame "delete frame")
    ("<left>" winner-undo "winner undo")
    ("<right>" winner-redo "winner redo"))))

(add-to-list 'aw-dispatch-alist '(?w hydra-window/body) t)

;;; winner mode
(winner-mode 1)
(keymap-sets winner-mode-map
             '(("C-c H" . winner-undo)
               ("C-c L" . winner-redo)))

;;; window rule
(cl-defun generate-display-buffer-alist (conditions &key popup (place 'bottom) (size 0.5) (slot 0))
  (when-let* ((conditions (if (listp conditions)
                              conditions
                            (list conditions))))
    (mapcar #'(lambda (condition)
                `(,(cond ((stringp condition)
                          condition)
                         ((symbolp condition)
                          (cons 'derived-mode condition))
                         (t
                          condition))
                  ,(if popup
                       '(display-buffer-pop-up-window)
                     '(display-buffer-in-side-window))
                  ,(pcase place
                     ((or 'bottom 'top)
                      (cons 'window-height size))
                     ((or 'left 'right)
                      (cons 'window-width size)))
                  (side . ,place)
                  (slot . ,slot)))
            conditions)))

(defun generate-popup-window (buffer-alists)
  (apply #'append
         (mapcar #'(lambda (buffer-alist)
                     (apply #'generate-display-buffer-alist buffer-alist))
                 buffer-alists)))

(add-hook 'auto-side-windows-mode-hook
          #'(lambda ()
              (when (bound-and-true-p auto-side-windows-mode)
                (add-list-to-list
                 'display-buffer-alist
                 (generate-popup-window
                  '((("*One-Key*" "*Org Agenda*"))))))))

;; (add-list-to-list
;;  'display-buffer-alist
;;  (generate-popup-window
;;   '(((help-mode helpful-mode fanyi-mode)
;;      :place right
;;      :size 75)

;;     (("*One-Key*" "*Org Agenda*"))
;;     (("\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*" "\\*Calendar\\*$")
;;      :size 0.25)

;;     (("\\*.*e?shell\\*" "^\\*.*vterm[inal]*.*\\*.*$" "*ielm*" sly-mrepl-mode)
;;      :popup t
;;      :place bottom
;;      :size 0.25
;;      :slot -1)

;;     ((compilation-mode cargo-process-mode)
;;      :place bottom
;;      :size 0.25
;;      :slot 0)

;;     (("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
;;       "\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
;;       "Output\\*$"
;;       "\\*ert\\*$"

;;       "\\*Async Shell Command\\*$"
;;       "\\*Apropos\\*$"
;;       "\\*Backtrace\\*$")
;;      :size 0.25
;;      :slot 1)

;;     (("\\*Find\\*$"
;;       "\\*Finder\\*$"
;;       "\\*Fd\\*$"

;;       "\\*Kill Ring\\*$"
;;       "\\*Embark \\(Collect\\|Live\\):.*\\*$"

;;       "\\*diff-hl\\**")
;;      :size 0.25
;;      :slot 2))))

(require 'auto-side-windows)
(setq auto-side-windows-bottom-buffer-names
      '("^\\*.*eshell.*\\*$" "^\\*.*shell.*\\*$" "^\\*.*term.*\\*$"
        "^\\*.*vterm.*\\*$"
        "*ielm*"

        "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*" "\\*Calendar\\*$"

        "\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
        "\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
        "Output\\*$"
        "\\*ert\\*$"

        "\\*Async Shell Command\\*$"
        "\\*Apropos\\*$"
        "\\*Backtrace\\*$"

        "\\*Find\\*$"
        "\\*Finder\\*$"
        "\\*Fd\\*$"

        "\\*Kill Ring\\*$"
        "\\*Embark \\(Collect\\|Live\\):.*\\*$"

        "\\*diff-hl\\**"
        ))
(setq auto-side-windows-bottom-buffer-modes
      '(eshell-mode shell-mode term-mode vterm-mode sly-mrepl-mode comint-mode
                    debugger-mode compilation-mode cargo-process-mode))
(setq auto-side-windows-bottom-alist
      '((window-height . 0.25)))


(setq auto-side-windows-right-buffer-names
      '("^\\*Metahelp\\*$"))
(setq auto-side-windows-right-buffer-modes
      '(help-mode helpful-mode fanyi-mode))
(setq auto-side-windows-right-alist
      '((window-width . 0.4)))

(setq auto-side-windows-common-window-parameters
      '((mode-line-format . nil)
        (header-line-format . nil)))

(setq window-sides-slots '(1 1 1 1))
(setq window-sides-vertical nil)

(auto-side-windows-mode 1)

;;; popper
(require 'popper)
(setq popper-group-function #'popper-group-by-directory
      popper-echo-dispatch-actions t
      popper-display-control nil)

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
