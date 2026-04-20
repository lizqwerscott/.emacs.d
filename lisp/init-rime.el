;;; init-rime.el --- init emacs rime package         -*- lexical-binding: t; -*-

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

(require 'rime)

(when sys/macp
  (custom-set-variables
   '(rime-librime-root "~/.emacs.d/librime/dist"))
  (custom-set-variables
   `(rime-emacs-module-header-root
     ,(if (file-exists-p "/Applications/Emacs.app/Contents/Resources/include/")
          "/Applications/Emacs.app/Contents/Resources/include/"
        "/opt/homebrew/include/"))))

(if sys/macp
    (setq rime-user-data-dir "~/Library/Rime")
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime/"))

(defun rime-predicate-org-in-src-block-p+ ()
  "Whether point is in an org-mode's code source block."
  (and (derived-mode-p 'org-mode)
       (org-in-src-block-p t)))

(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(meow-normal-mode-p
        meow-motion-mode-p
        meow-keypad-mode-p
        meow-beacon-mode-p

        rime-predicate-hydra-p
        rime-predicate-ace-window-p

        rime-predicate-prog-in-code-p
        rime-predicate-org-in-src-block-p+
        rime-predicate-org-latex-mode-p

        rime-predicate-punctuation-line-begin-p

        rime-predicate-after-alphabet-char-p
        rime-predicate-punctuation-after-space-cc-p))

(setq rime-inline-predicates
      ;; If cursor is after a whitespace
      ;; which follow a non-ascii character.
      '(rime-predicate-space-after-cc-p
        ;; If the current charactor entered is a uppercase letter.
        rime-predicate-current-uppercase-letter-p))

(require 'im-cursor-chg)
(cursor-chg-mode t)

(defun rime-candidates-regexp (component)
  "Match COMPONENT as a chinese regexp."
  (unless rime--lib-loaded
    (unless (file-exists-p rime--module-path)
      (rime-compile-module))
    (rime--load-dynamic-module))
  (let ((candidates (rime-lib-get-candidates-for-input component 100)))
    (if candidates
        (condition-case nil
            (cons
             (concat "\\("
                     (mapconcat (lambda (cand)
                                  (regexp-quote cand))
                                candidates
                                "\\|")
                     "\\)")
             candidates)
          (invalid-regexp nil))
      (list "\\cc"))))

(defun rime-chinese-orderless-regexp (component)
  "Match COMPONENT as a chinese regexp."
  (car (rime-candidates-regexp component)))

(with-eval-after-load 'orderless
  (add-to-list 'orderless-affix-dispatch-alist
               `(?= . ,#'rime-chinese-orderless-regexp)))

(provide 'init-rime)
;;; init-rime.el ends here
