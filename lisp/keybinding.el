;;; keybinding.el --- project                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

(defun run-or-compile ()
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-switch-mrepl)
    (if (equal major-mode 'python-mode)
        (let ((command (concat "python "
                               (file-truename (buffer-name)))))
          (setq command (compilation-read-command command))
          (vterm-run command))
      (vterm-compile))))

(defun my/meow-quit ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (kill-this-buffer)
    (if (meow-quit)
        (message "finish"))))

(defun help-helfup-lsp-bridge-sly ()
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (equal major-mode 'emacs-lisp-mode)
        (helpful-at-point)
      (if (bound-and-true-p lsp-bridge-mode)
          (lsp-bridge-lookup-documentation)
        (message "dont't know how to help")))))

(defun open-main-directory ()
  "Open main directory use eaf file mamanger."
  (interactive)
  (eaf-open "~/"))

(defun open-big-screen-mode ()
  "Start big screen mode."
  (interactive)
  (imenu-list-smart-toggle))

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

  ;;Another command
  (meow-leader-define-key
   '("tt" . gts-do-translate)
   '("th" . helpful-at-point)
   '("tg" . google-this)
   '("tp" . popweb-dict-bing-pointer))
  ;;'("m" . vc-msg-show)

  ;;Big Screen
  (meow-leader-define-key
   '("l" . open-big-screen-mode))

  ;;lsp bridge
  (meow-leader-define-key
   '("rn" . lsp-bridge-rename)
   '("aa" . lsp-bridge-diagnostic-list)
   ;; '("aa" . consult-flycheck)
   '("ah" . lsp-bridge-popup-documentation)
   '("an" . lsp-bridge-diagnostic-jump-next)
   '("ap" . lsp-bridge-diagnostic-jump-prev)
   ;; '("an" . flycheck-next-error)
   ;; '("ap" . flycheck-previous-error)
   )


  ;;window key
  (meow-leader-define-key
   '("o" . other-window)
   '("ag" . avy-goto-line)
   '("1" . ace-delete-other-windows)
   '("2" . split-window-below)
   '("-" . split-window-horizontally)
   '("/" . split-window-vertically)
   '("3" . split-window-horizontally)
   '("0" . ace-delete-window))

  ;;run file
  (meow-leader-define-key
   '("r" . run-or-compile))

  (meow-leader-define-key
   '("j" . blink-search))

  ;;consult and file
  (meow-leader-define-key
   '("sl" . consult-line)
   '("si" . consult-imenu)
   '("sm" . consult-imenu-multi)
   '("sg" . consult-goto-line)
   '("so" . consult-outline))

  ;;file
  (meow-leader-define-key
   '("f" . find-file)
   ;; '("fr" . consult-find)
   '("sf" . ff-find-other-file))


  ;;buffer
  (meow-leader-define-key
   '("b" . switch-to-buffer))

  ;;bookmakr
  (meow-leader-define-key
   '("Bs" . bookmark-set)
   '("Bj" . consult-bookmark))

  ;;dired
  (meow-leader-define-key
   ;; '("dj" . dired-jump)
   ;; '("dJ" . dired-jump-other-window)
   '("d" . open-main-directory))

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
   '("C-o" . return-find-def)
   '("/" . consult-ripgrep)
   '("?" . help-helfup-lsp-bridge-sly)))


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

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(global-set-key (kbd "s-x") #'execute-extended-command)

(provide 'keybinding)
;;; keybinding.el ends here.
