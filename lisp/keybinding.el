;;; keybinding.el --- project                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

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

(defun run-or-compile ()
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-switch-mrepl)
    (progn
      (require 'init-vterm)
      (if (equal major-mode 'python-mode)
          (let ((command (concat "python "
                                 (file-truename (buffer-name)))))
            (setq command (compilation-read-command command))
            (vterm-run command))
        (vterm-compile)))))

(defun my/meow-quit ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (kill-this-buffer)
    (if (delete-window)
        (message "finish"))))

(defun help-helfup-lsp-bridge-sly ()
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (or (equal major-mode 'emacs-lisp-mode)
           (equal major-mode 'lisp-interaction-mode))
        (helpful-at-point)
      (if (bound-and-true-p lsp-bridge-mode)
          (lsp-bridge-popup-documentation)
        (message "dont't know how to help")))))

(defun open-main-directory ()
  "Open main directory use eaf file mamanger."
  (interactive)
  (eaf-open "~/"))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '("<escape>" . ignore))

  (lazy-meow-leader-define-key
   '(("p" . one-key-menu-project) "init-project")
   '(("e" . one-key-menu-eaf) "init-eaf")
   )
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
   '("t" . one-key-menu-toggle)
   '("u" . one-key-menu-useful)
   '("j" . one-key-menu-code)
   '("s" . one-key-menu-search)
   '("f" . one-key-menu-file)
   '("b" . one-key-menu-buffer)
   '("w" . one-key-menu-org)
   '("v" . one-key-menu-sort-tab)
   '("d" . one-key-menu-directory)
   )

  ;;window key
  (meow-leader-define-key
   '("ag" . avy-goto-line)
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("-" . split-window-horizontally)
   '("/" . split-window-vertically)
   '("3" . split-window-horizontally)
   '("0" . delete-window))

  ;;run file
  (meow-leader-define-key
   '("r" . run-or-compile))

  (lazy-meow-insert-define-key
   '(("C-c i" . insert-translated-name-insert) "init-translated-name"))

  ;;bookmakr
  ;; (meow-leader-define-key
  ;;  '("Bs" . bookmark-set)
  ;;  '("Bj" . consult-bookmark))

  ;;dired
  ;;(meow-leader-define-key
  ;; '("dj" . dired-jump)
  ;; '("dJ" . dired-jump-other-window)
  ;; '("d" . open-main-directory)
  ;;)

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
   '("." . meow-inner-of-thing)
   '("," . meow-bounds-of-thing)
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
   '("N" . lizqwer/mark-line-comment)
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

  (meow-normal-define-key
   '("C-s" . save-buffer)
   '("C-y" . meow-clipboard-yank)
   '("Q" . kill-this-buffer)
   '("gr" . lsp-bridge-find-references)
   '("gd" . find-definition-with-lsp-bridge)
   '("gf" . find-file-at-point)
   '("gi" . lsp-bridge-find-implementation)
   '("gp" . goto-percent)
   '("C-o" . return-find-def)
   '("/" . consult-ripgrep)
   '("?" . help-helfup-lsp-bridge-sly)
   '("M-j" . scroll-up)
   '("M-k" . scroll-down)))


(use-package meow
  :ensure t
  :init
  (meow-global-mode 1)
  :custom
  (meow-esc-delay 0.001)
  :config
  (setq meow-keypad-leader-dispatch "C-c")
  (setq meow-mode-state-list
        '((fundamental-mode . normal)
          (text-mode . normal)
          (prog-mode . normal)
          (conf-mode . normal)
          (eaf-mode . insert)))
  (meow-setup))

(provide 'keybinding)
;;; keybinding.el ends here.
