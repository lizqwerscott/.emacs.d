;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)
                (call-interactively #'fingertip-mode)))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
(require 'init-fingertip)

;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-sets nxml-mode-map
    '(("C-s-f" . nxml-down-element)
      ("C-s-n" . nxml-forward-element)
      ("C-s-p" . nxml-backward-element)
      ("C-s-b" . nxml-backward-up-element))))

;;; aggressive-indent
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode)

;;; indent yank
(require 'indent-yank)
(add-hook 'prog-mode-hook
          #'indent-yank-mode)
(indent-yank-mode 1)

;;; Visual Replace
(require 'visual-replace)
(setq visual-replace-min-length 1)

(transient-define-prefix visual-replace-dispatch ()
  "Visual replace menu."
  ["Toggles"
   ("r" "Regexp" visual-replace-toggle-regexp)
   ("s" "Scope" visual-replace-toggle-scope)
   ("q" "Query" visual-replace-toggle-query)
   ("w" "Word" visual-replace-toggle-word)
   ("c" "Case Fold" visual-replace-toggle-case-fold)
   ("l" "Lax ws" visual-replace-toggle-lax-ws)])

(keymap-set visual-replace-mode-map
            "C-o" #'visual-replace-dispatch)
(global-set-key (kbd "s-r") #'visual-replace)
(visual-replace-global-mode 1)

;;; isearch
(setq isearch-lazy-count t
      isearch-case-fold-search t
      lazy-count-prefix-format "%s/%s "
      search-whitespace-regexp ".*?")

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line query)))

(with-eval-after-load 'isearch
  (keymap-sets isearch-mode-map
    '(("<escape>" . isearch-exit)))

  (with-eval-after-load 'casual-isearch
    (transient-define-suffix isearch-consult-line ()
      (interactive)
      (call-interactively #'my-isearch-consult-line-from-isearch))
    (transient-append-suffix 'casual-isearch-tmenu "u"
      '("c" "Use consult line" isearch-consult-line))))

;;; Outline indent
(require 'init-outline-indent)

;;; Auto rename tag
(add-hooks '(html-mode web-mode)
           #'auto-rename-tag-mode)

(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

;;; insert trailing semi
(defun insert-or-remove-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (if (eq (char-before) ch)
                  (delete-backward-char 1)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-or-remove-trailing-semi ()
  (interactive)
  (insert-or-remove-trailing-char ?\;))

(defun insert-or-remove-trailing-comma ()
  (interactive)
  (insert-or-remove-trailing-char ?,))

(defun insert-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (unless (eq (char-before) ch)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-trailing-semi ()
  (interactive)
  (insert-trailing-char ?\;))

(defun insert-trailing-semi-and-indent ()
  (interactive)
  (insert-trailing-char ?\;)
  (forward-char)
  (newline-and-indent))

;;; grugru
(grugru-default-setup)

(grugru-define-multiple
  ((c-ts-mode c++-ts-mode)
   (non-alphabet "&&" "||")
   (non-alphabet "&" "|")
   (non-alphabet "+" "-")
   (non-alphabet "*" "/" "%")
   (non-alphabet ">>" "<<")
   (non-alphabet "+=" "-=")
   (non-alphabet "*=" "/=" "%=")
   (non-alphabet "&=" "|=" "^=")
   (non-alphabet ">>=" "<<=")
   (non-alphabet "++" "--")
   (non-alphabet "==" "!=")
   (non-alphabet "<" "<=" ">" ">=")
   (symbol "break" "continue")
   (symbol "signed" "unsigned"))
  (c++-ts-mode
   (symbol "true" "false")
   (symbol "vector" "array" "deque")
   (symbol "class" "struct")
   (symbol "float" "double")
   (symbol "private" "public" "protected")
   (symbol "pair" "tuple")
   (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
  ((python-mode python-ts-mode)
   (symbol "True" "False"))
  ((emacs-lisp-mode lisp-mode)
   (symbol "when-let" "if-let")))

;;; add rectangle number lines
;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;; goto precent
;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun scroll-other-window-up-1/3 ()
  (interactive)
  (scroll-other-window (/ (window-body-height) 3)))

(defun scroll-other-window-down-1/3 ()
  (interactive)
  (scroll-other-window-down (/ (window-body-height) 3)))

;;;###autoload
(defun toggle-sub-word-or-super-word ()
  (interactive)
  (if subword-mode
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(provide 'init-edit)
;;; init-edit.el ends here.
