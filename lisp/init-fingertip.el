;;; init-fingertip.el --- init fingertip package     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(dolist (hook (list
               'bash-ts-mode-hook
               'c++-mode-hook
               'c++-ts-mode-hook
               'c-mode-common-hook
               'c-mode-hook
               'c-ts-mode-hook
               'cmake-ts-mode-hook
               'coffee-mode-hook
               'conf-toml-mode-hook
               'css-mode-hook
               'css-ts-mode-hook
               'emacs-lisp-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'ielm-mode-hook
               'jade-mode-hook
               'java-mode-hook
               'js-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'kotlin-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'llvm-mode-hook
               'lua-mode-hook
               'makefile-gmake-mode-hook
               'markdown-ts-mode-hook
               'maxima-mode-hook
               'mojo-mode-hook
               'nim-mode-hook
               'php-mode-hook
               'php-ts-mode-hook
               'python-mode-hook
               'python-ts-mode-hook
               'qmake-mode-hook
               'qml-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'rust-ts-mode-hook
               'sh-mode-hook
               'swift-mode-hook
               'toml-ts-mode-hook
               'typescript-mode-hook
               'typescript-ts-mode-hook
               'web-mode-hook
               'zig-mode-hook
               'fsharp-mode-hook
               'clojure-mode-hook
               'clojure-ts-mode-hook
               'cider-repl-mode-hook
               'nix-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (when (or
                            (not (buffer-file-name))
                            (not (string-equal (file-name-extension (buffer-file-name)) "chat")))
                       (fingertip-mode 1)))))
(require 'fingertip)

(define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
(define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
(define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
(define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
(define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
(define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
(define-key fingertip-mode-map (kbd "=") 'fingertip-equal)

(define-key fingertip-mode-map (kbd "（") 'fingertip-open-chinese-round)
(define-key fingertip-mode-map (kbd "「") 'fingertip-open-chinese-bracket)
(define-key fingertip-mode-map (kbd "【") 'fingertip-open-chinese-curly)
(define-key fingertip-mode-map (kbd "）") 'fingertip-close-chinese-round)
(define-key fingertip-mode-map (kbd "」") 'fingertip-close-chinese-bracket)
(define-key fingertip-mode-map (kbd "】") 'fingertip-close-chinese-curly)

(define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
(define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
(define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

(define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
;; (define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

(define-key fingertip-mode-map (kbd "M-d") 'fingertip-backward-delete)
(define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
(define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

(define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
(define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
(when (display-graphic-p)
  (define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket))
(define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
(define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
(define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

(define-key fingertip-mode-map (kbd "s-\"") 'fingertip-wrap-double-quote)
(define-key fingertip-mode-map (kbd "s-'") 'fingertip-wrap-single-quote)
(when (display-graphic-p)
  (define-key fingertip-mode-map (kbd "s-[") 'fingertip-wrap-bracket))
(define-key fingertip-mode-map (kbd "s-{") 'fingertip-wrap-curly)
(define-key fingertip-mode-map (kbd "s-(") 'fingertip-wrap-round)
(define-key fingertip-mode-map (kbd "s-)") 'fingertip-unwrap)

(define-key fingertip-mode-map (kbd "M-C-p") 'fingertip-jump-right)
(define-key fingertip-mode-map (kbd "M-C-n") 'fingertip-jump-left)
(define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

(define-key fingertip-mode-map (kbd "s-C-p") 'fingertip-jump-right)
(define-key fingertip-mode-map (kbd "s-C-n") 'fingertip-jump-left)
(define-key fingertip-mode-map (kbd "s-:") 'fingertip-jump-out-pair-and-newline)

(define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up)



(provide 'init-fingertip)
;;; init-fingertip.el ends here
