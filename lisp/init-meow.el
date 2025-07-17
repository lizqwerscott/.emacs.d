(require 'meow)
(setq meow-esc-delay 0.001)
(setq meow-keypad-leader-dispatch "C-c")
(setq meow-mode-state-list
      '((fundamental-mode . normal)
        (text-mode . normal)
        (prog-mode . normal)
        (conf-mode . normal)
        (elfeed-show-mode . normal)
        (helpful-mode . normal)
        (help-mode . normal)
        (cargo-process-mode . normal)
        (compilation-mode . normal)
        (messages-buffer-mode . normal)
        (eww-mode . normal)
        (color-rg-mode . insert)
        (rg-mode . insert)
        (lsp-bridge-ref-mode . insert)
        (vterm-mode . insert)
        (Info-mode-hook . motion)))
(setq meow-use-clipboard t)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(meow-thing-register 'url 'url 'url)
(require 'mark-comment)
(meow-thing-register 'comment #'mark-comment-inner-of-comment #'mark-comment-inner-of-comment)

(setq wrap-keymap (let ((map (make-keymap)))
                    (suppress-keymap map)
                    (dolist (k '("(" "[" "{" "<"))
                      (define-key map k #'insert-pair))
                    map)
      meow-char-thing-table '((?\( . round)
                              (?\) . round)
                              (?\g .  string)
                              (?\[ . square)
                              (?\] . square)
                              (?<  . angle)
                              (?>  . angle)
                              (?{  . curly)
                              (?}  . curly)
                              (?s  . symbol)
                              (?d  . defun)
                              (?w  . window)
                              (?l  . line)
                              (?b  . buffer)
                              (?p  . paragraph)
                              (?u . url)
                              (?c . comment)))
(meow-normal-define-key (cons "\\" wrap-keymap))

(defun lazy-meow-leader-define-key (&rest keybinds)
  (let* ((meow-leader-keybinds))
    (dolist (ele  keybinds)
      (let ((func (cdar ele))
            (key (caar ele))
            (filename (cadr ele)))
        (autoload func filename nil t)
        (meow-define-keys 'leader (cons key func))))))

(defun lazy-meow-insert-define-key (&rest keybinds)
  (let* ((meow-insert-keybinds))
    (dolist (ele  keybinds)
      (let ((func (cdar ele))
            (key (caar ele))
            (filename (cadr ele)))
        (autoload func filename nil t)
        (meow-define-keys 'insert (cons key func))))))

(one-key-create-menu
 "Rust"
 '((("r" . "Rust Run") . cargo-process-run)
   (("c" . "Rust check") . cargo-process-clippy)
   (("b" . "Rust Compile") . cargo-process-build)))

(defun run-or-compile ()
  "Run or compile this project or file."
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-switch-mrepl)
    (autoload 'project-root-path "init-project" nil t)
    (let ((project-path (project-root-path)))
      (pcase major-mode
        ((or 'python-ts-mode 'python-mode)
         (let ((command (concat (if project-path
                                    (cond ((file-exists-p (file-name-concat project-path "uv.lock")) "uv run")
                                          ((file-exists-p (file-name-concat project-path "pdm.lock")) "pdm run")
                                          (t "python"))
                                  "python")
                                " "
                                (file-truename (buffer-file-name)))))
           (setq command (compilation-read-command command))
           (require 'multi-vterm)
           (multi-vterm-run command)))
        ((or 'rust-ts-mode 'rust-mode) (one-key-menu-rust))
        ('haskell-mode
         (progn
           (setq command
                 (compilation-read-command
                  (concat "cabal run")))
           (require 'multi-vterm)
           (multi-vterm-run command)))
        ((or 'zig-mode 'zig-ts-mode)
         (let ((command (if project-path
                            "zig build run"
                          (concat "zig run "
                                  (file-truename (buffer-file-name))))))
           (setq command (compilation-read-command command))
           (require 'multi-vterm)
           (multi-vterm-run command)))
        ('emacs-lisp-mode (eval-buffer))
        (_ (if project-path
               (call-interactively #'projection-commands-build-project)
             (call-interactively #'compile)))))))

(defun kill-now-buffer ()
  "Close the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun help-helpful-lsp-sly ()
  "Help function with lsp and sly info"
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (or (equal major-mode 'emacs-lisp-mode)
           (equal major-mode 'lisp-interaction-mode))
        (helpful-at-point)
      (pcase user/lsp-client
        ('eglot
         (eldoc-box-help-at-point))
        ('lsp-bridge
         (if (bound-and-true-p lsp-bridge-mode)
             (lsp-bridge-popup-documentation)
           (message "dont't know how to help")))))))

(defun find-references-with-sly ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-uses))
   (t
    (xref-find-references (thing-at-point 'symbol)))))

(defun find-definition-with-sly ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition))
   (t
    (xref-find-definitions (thing-at-point 'symbol)))))

(defun find-definition-with-sly-other-window ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-edit-definition-other-window))
   (t
    (xref-find-definitions-other-window (thing-at-point 'symbol)))))

(defun return-find-def-with-sly ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (call-interactively #'sly-pop-find-definition-stack))
   (t
    (call-interactively #'xref-go-back))))

(defun find-implementation-with-sly ()
  (interactive)
  (cond
   ((bound-and-true-p sly-mode)
    (sly-who-calls (thing-at-point 'symbol)))
   ((bound-and-true-p eglot-mode)
    (call-interactively #'eglot-find-implementation))
   (t
    (message "This mode not support implementation."))))

(defun match-in (pred lst)
  (catch 'found
    (dolist (x lst)
      (when (funcall pred x)
        (throw 'found t)))))

(defun my/meow-quit ()
  (interactive)
  (if (match-in #'(lambda (regex)
                    (buffer-match-p (if (symbolp regex)
                                        (cons 'derived-mode regex)
                                      regex)
                                    (buffer-name)))
                popper-reference-buffers)
      (popper--delete-popup (selected-window))
    (meow-quit)))

(defun meow-setup ()
  ;; (meow-motion-overwrite-define-key
  ;;  '("j" . meow-next)
  ;;  '("k" . meow-prev)
  ;;  '("h" . meow-left)
  ;;  '("l" . meow-right)
  ;;  '("o" . meow-block)
  ;;  '("x" . meow-line)
  ;;  '("W" . meow-mark-symbol)
  ;;  '("w" . meow-mark-word)
  ;;  '("C-j" . (lambda ()
  ;;              (interactive)
  ;;              (dotimes (i 2)
  ;;             (call-interactively 'meow-next))))
  ;;  '("C-k" . (lambda ()
  ;;              (interactive)
  ;;              (dotimes (i 2)
  ;;             (call-interactively 'meow-prev))))
  ;;  '("<escape>" . ignore))

  (meow-leader-define-key
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
   '("w" . hydra-window/body)
   '("t" . hydra-toggles/body)
   '("u" . one-key-menu-useful)
   '("j" . one-key-menu-code)
   '("s" . one-key-menu-search)
   '("f" . one-key-menu-file)
   '("b" . one-key-menu-buffer)
   '("o" . one-key-menu-org)
   '("v" . hydra-git/body)
   '("l" . one-key-menu-workspace)
   '("z" . hydra-language/body)
   '("d" . hydra-jump-dir/body)
   '("i" . one-key-menu-insert)
   '("a" . one-key-menu-agenda)
   '("n" . one-key-menu-roam)
   )

  (lazy-meow-leader-define-key
   '(("p" . project-dispatch) "init-project"))

  (meow-leader-define-key
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-horizontally)
   '("0" . delete-window)
   '("r" . run-or-compile))

  (meow-define-keys 'insert
    '("C-c i" . one-key-menu-insert))

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
   '("P" . meow-yank-pop)
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
   ;; '("Y" . meow-sync-grab)
   '("Y" . meow-clipboard-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (pcase user/lsp-client
    ('eglot
     (meow-normal-define-key
      '("gr" . find-references-with-sly)
      '("gd" . find-definition-with-sly)
      '("gD" . find-definition-with-sly-other-window)
      '("gi" . find-implementation-with-sly)
      '("C-o" . return-find-def-with-sly)))
    ('lsp-bridge
     (meow-normal-define-key
      '("gr" . lsp-bridge-find-references)
      '("gd" . find-definition-with-lsp-bridge)
      '("gD" . find-definition-with-lsp-bridge-other-window)
      '("gi" . lsp-bridge-find-impl)
      '("gI" . lsp-bridge-find-impl-other-window)
      '("C-o" . return-find-def))))

  (meow-normal-define-key
   '("C-;" . grugru)
   '("C-y" . meow-clipboard-yank)
   '("Q" . kill-now-buffer)
   '("?" . help-helpful-lsp-sly)
   '("gf" . find-file-at-point)
   '("gp" . goto-percent)
   '("gl" . consult-goto-line)
   '("/" . consult-ripgrep)))

(meow-vterm-enable)
(meow-setup)
(meow-global-mode 1)

;; (require 'meow-tree-sitter)
;; (meow-tree-sitter-register-defaults)

(setq repeat-fu-preset 'meow)
(add-hook 'meow-mode-hook
          #'(lambda ()
              (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
                (repeat-fu-mode)
                (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
                (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute))))

(provide 'init-meow)
