;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

(require 'lib-edit)

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate #'electric-pair-default-inhibit)

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?\{ . ?\})))

(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)
                ;; (call-interactively #'fingertip-mode)
                ))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
;; (require 'init-fingertip)

;;; puni
(require 'init-puni)

;;; vundo
(require 'vundo)
(setq undo-limit (* 1 1024 1024))
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-binds nxml-mode-map
    ("C-s-f" . nxml-down-element)
    ("C-s-n" . nxml-forward-element)
    ("C-s-p" . nxml-backward-element)
    ("C-s-b" . nxml-backward-up-element)))

;;; aggressive-indent
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode)

;;; indent yank
(require 'indent-yank)
(add-hooks '(python-mode python-ts-mode)
           #'indent-yank-mode)

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
      search-whitespace-regexp ".*?"
      isearch-repeat-on-direction-change t
      isearch-wrap-pause nil)

(defun my-occur-from-isearch ()
  "Generate occur from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (occur query)))

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

(defun isearch-yank-thing-at-point ()
  "Isearch yank thing at point."
  (interactive)
  (if-let* ((word (thing-at-point 'word)))
      (isearch-yank-string word)
    (if-let* ((symbol (thing-at-point 'symbol)))
        (isearch-yank-string word))))

(defun isearch-yank-thing-at-point-and-forward ()
  "Isearch yank thing at point and isearch forward."
  (interactive)
  (when (isearch-yank-thing-at-point)
    (isearch-repeat-forward)))

(with-eval-after-load 'isearch
  (keymap-binds isearch-mode-map
    ("<escape>" . isearch-exit)
    ("s-r" . isearch-toggle-regexp)
    ("s-e" . isearch-edit-string)

    ("C-w" . isearch-yank-thing-at-point-and-forward)

    ("C-v" . visual-replace-from-isearch)
    ("C-o" . my-occur-from-isearch)
    ("C-j" . my-isearch-consult-line-from-isearch)))

;; repeat for isearch
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;;; Outline indent
(with-eval-after-load 'outline-indent
  (setq outline-indent-ellipsis " ▼"))

(add-hooks '(yaml-ts-mode nxml-mode)
           #'outline-indent-minor-mode)

;;; Auto rename tag
(add-hooks '(html-mode web-mode)
           #'auto-rename-tag-mode)

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

(with-eval-after-load 'meow
  (meow-normal-define-key
   '("C-;" . grugru)))

;;; bufferfile
(lazy-load-global-keys
 '(("C-x x r" . bufferfile-rename)
   ("C-x x d" . bufferfile-delete))
 "init-bufferfile")

(autoload #'bufferfile-dired-do-rename "bufferfile" nil t)

;;; file info
(global-bind-keys
 ("C-x x I" . file-info-show))

;;; rg
(lazy-load-global-keys
 '(("M-s s" . rg-dwim)
   ("M-s R" . rg-menu)
   ("M-s r" . rg)
   ("M-s t" . rg-literal)
   ("M-s p" . rg-project))
 "init-rg")

;;; symbol overlay
(lazy-load-global-keys
 '(("M-i" . symbol-overlay-put)
   ("s-i" . symbol-overlay-put)
   ("M-g n" . symbol-overlay-jump-next)
   ("M-g p" . symbol-overlay-jump-prev))
 "symbol-overlay")

;;; keymap

(keymap-unset ctl-x-map "C-k")
(keymap-unset ctl-x-map "C-x")

(global-bind-keys
 ("RET" . newline-and-indent)
 (("S-<return>" "C-<return>") . comment-indent-new-line)
 (("s-n" "M-n") . scroll-up-1/3)
 (("s-p" "M-p") . scroll-down-1/3)
 (("M-N" "s-N") . scroll-other-window-up-1/3)
 (("M-P" "s-P") . scroll-other-window-down-1/3)

 ("C-x C-n" . next-buffer)
 ("C-x C-p" . previous-buffer)

 ("C-x k" . kill-current-buffer)
 ("C-x K" . kill-buffer)

 ("C-x x b" . browse-this-file)

 ("M-g P" . goto-percent)

 ("C-s-f" . forward-sexp)
 ("C-s-b" . backward-sexp)
 ("s-\\" . cycle-spacing))

(with-eval-after-load 'init-meow
  (meow-normal-define-key
   '("C-c <" . remember-init)
   '("C-c >" . remember-jump)))

;; repeat for scroll up
(defvar-keymap scroll-repeat-map
  :repeat t
  "n" #'scroll-up-1/3
  "p" #'scroll-down-1/3)

(defvar-keymap scroll-other-window-repeat-map
  :repeat t
  "n" #'scroll-other-window-up-1/3
  "p" #'scroll-other-window-down-1/3)

;;; editkit
(autoload #'editkit-rectangle-menu "editkit" nil t)

(global-bind-keys
 ("C-c m" . editkit-rectangle-menu))

(provide 'init-edit)
;;; init-edit.el ends here.
