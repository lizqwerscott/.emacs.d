(require 'awesome-pair)

(add-hooks '(web-mode
             c-mode
             c++-mode
             haskell-mode
             emacs-lisp-mode
             lisp-interaction-mode
             lisp-mode
             ielm-mode
             python-mode
             css-mode
             rust-mode
             minibuffer-inactive-mode
             jsonian-mode
             csharp-mode)
           #'awesome-pair-mode)

(add-hooks '(vue-ts-mode
             rust-ts-mode
             c-ts-mode
             c++-ts-mode
             python-ts-mode
             css-ts-mode
             go-ts-mode
             csharp-ts-mode)
           #'awesome-pair-mode)

(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
(define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)

(define-key awesome-pair-mode-map (kbd "M-d") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

(define-key awesome-pair-mode-map (kbd "s-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "s-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "s-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "s-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "s-)") 'awesome-pair-unwrap)

(define-key awesome-pair-mode-map (kbd "M-C-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-C-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

(define-key awesome-pair-mode-map (kbd "C-s-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "C-s-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "s-:") 'awesome-pair-jump-out-pair-and-newline)

(provide 'init-awesome-pair)
