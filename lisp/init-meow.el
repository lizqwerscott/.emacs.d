;;; init-meow.el ---                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'meow)
(setq meow-keypad-meta-prefix nil)
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
        (rg-mode . insert)
        (lsp-bridge-ref-mode . insert)
        (vterm-mode . insert)
        (Info-mode-hook . motion)))
(setq meow-use-clipboard t)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(meow-thing-register 'url 'url 'url)
(meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))

(require 'mark-comment)
(meow-thing-register 'comment #'mark-comment-inner-of-comment #'mark-comment-inner-of-comment)

(defvar wrap-keymap
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (dolist (k '("(" "[" "{" "<"))
      (define-key map k #'insert-pair))
    map)
  "Keymap for wrap.")

(meow-normal-define-key (cons "\\" wrap-keymap))

(setq meow-char-thing-table '((?\( . round)
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

(defun my/meow-quit ()
  "Meow quit."
  (interactive)
  (if (match-in #'(lambda (regex)
                    (buffer-match-p (if (symbolp regex)
                                        (cons 'derived-mode regex)
                                      regex)
                                    (buffer-name)))
                popper-reference-buffers)
      (popper--delete-popup (selected-window))
    (meow-quit)))

(keymap-sets goto-map
  '(("f" . find-file-at-point)))

(defvar-keymap find-map
  :doc "Keymap for find commands."
  "c" #'find-custom-file
  "l" #'find-library
  "v" #'find-variable)

(defalias 'find-map find-map)

(lazy-load-global-keys
 '(("C-c p" . project-dispatch))
 "init-project")

(defun meow-setup ()
  "Meow keymap setup."
  (meow-leader-define-key
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-leader-define-key
   '("f" . find-file)
   '("F" . find-file-other-window))

  (meow-leader-define-key
   '("s" . "M-s"))

  (meow-leader-define-key
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-horizontally)
   '("0" . delete-window))

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
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill)
   '("D" . meow-kill-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-cancel-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("y" . meow-save)
   '("Y" . meow-clipboard-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("g" . "M-g")
   (cons "F" find-map))

  (meow-normal-define-key
   '("C-;" . grugru)
   '("Q" . kill-buffer-and-window)
   '("?" . "C-h ?")
   '("/" . consult-ripgrep)))

(meow-vterm-enable)
(meow-setup)
(meow-global-mode 1)

(global-set-keys
 '(("C-y" . meow-clipboard-yank)))

;; (require 'meow-tree-sitter)
;; (meow-tree-sitter-register-defaults)

(require 'repeat-fu)
(setq repeat-fu-preset 'meow)
(add-hook 'meow-mode-hook
          #'(lambda ()
              (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
                (repeat-fu-mode)
                (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
                (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute))))

(provide 'init-meow)
;;; init-meow.el ends here
