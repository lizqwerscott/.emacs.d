;; A minimum .emacs config to test Emacs plugins
(show-paren-mode 1)
(eval-when-compile (require 'cl))

;; test elisps download from internet here
;; (setq test-elisp-dir "~/test-elisp/")
;; (unless (file-exists-p (expand-file-name test-elisp-dir))
;;     (make-directory (expand-file-name test-elisp-dir)))

;; (setq load-path
;;       (append
;;         (loop for dir in (directory-files test-elisp-dir)
;;               unless (string-match "^\\." dir)
;;               collecting (expand-file-name (concat test-elisp-dir dir)))
;;         load-path))

;; package repositories
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(package-initialize)

;; ==== put your code below this line!
;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")

;; (require 'yasnippet)
;; (yas-global-mode 1)

(require 'lsp-bridge)
(setq lsp-bridge-c-lsp-server "ccls")
(global-lsp-bridge-mode)

(defun meow-setup ()
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
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/awesome-pair")
;; (require 'awesome-pair)
;; (dolist (hook (list
;;                ;; 'c-mode-common-hook
;;                ;; 'c-mode-hook
;;                ;; 'c++-mode-hook
;;                'haskell-mode-hook
;;                'emacs-lisp-mode-hook
;;                'lisp-interaction-mode-hook
;;                'lisp-mode-hook
;;                'common-lisp-mode-hook
;;                'maxima-mode-hook
;;                'sh-mode-hook
;;                'python-mode-hook
;;                'js-mode-hook
;;                'go-mode-hook
;;                'css-mode-hook
;;                'ruby-mode-hook
;;                'rust-mode-hook
;;                'lua-mode-hook
;;                'swift-mode-hook
;;                'minibuffer-inactive-mode-hook
;;                ))
;;   (add-hook hook
;;             #'(lambda ()
;;                 (awesome-pair-mode 1))))

;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
;; (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
;; (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
;; (define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)
