;;; init-pyim.el --- init pyim package               -*- lexical-binding: t; -*-

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

(require 'posframe)

;;; Copy from https://github.com/DogLooksGood/emacs-rime/blob/fd434071ce95c41e5d580e303ccf2a65f189e7ec/rime-predicates.el#LL14C1-L20C90
(defun rime-predicate-after-alphabet-char-p ()
  "If the cursor is after a alphabet character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
     (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
       (string-match-p "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*$" string))))

;; (defun rime-predicate-after-alphabet-char-p ()
;;   "If the cursor is after a alphabet character.

;; Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
;;   (and (> (point) (save-excursion (back-to-indentation) (point)))
;;      (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
;;        (string-match-p "[A-Z]*$" string))))


(defun rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
     (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
       (string-match-p "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]$" string))))

(defun rime-predicate-space-after-ascii-p ()
  "If cursor is after a whitespace which follow a ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
     (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
       (and (string-match-p " +$" string)
          (not (string-match-p "\\cc +$" string))))))

(defun rime-predicate-space-after-cc-p ()
  "If cursor is after a whitespace which follow a non-ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
     (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
       (string-match-p "\\cc +$" string))))

(require 'pyim)
;; (pyim-default-scheme 'quanpin)
(pyim-default-scheme 'xiaohe-shuangpin)
(setq pyim-cloudim 'baidu)
(setq pyim-page-tooltip 'posframe)
(setq-default pyim-english-input-switch-functions
              `(
                ;; pyim-probe-dynamic-english
                rime-predicate-after-alphabet-char-p
                ;; rime-predicate-after-ascii-char-p
                ;; rime-predicate-space-after-ascii-p
                rime-predicate-space-after-cc-p

                meow-normal-mode-p
                meow-motion-mode-p
                meow-keypad-mode-p

                pyim-probe-program-mode
                pyim-probe-org-structure-template))
(setq-default pyim-punctuation-translate-p '(no))
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)
(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)

(global-set-key (kbd "C-\\") 'toggle-input-method)

(defun chinese-orderless-regexp (component)
  "Match COMPONENT as a chinese regexp."
  (condition-case nil
      (pyim-cregexp-build
       (progn (string-match-p component "")
              component))
    (invalid-regexp nil)))

(with-eval-after-load 'orderless
  (add-to-list 'orderless-affix-dispatch-alist
               `(?= . ,#'chinese-orderless-regexp)))

(require 'pyim-cstring-utils)
(global-bind-keys
 (("M-f" "s-f") . pyim-forward-word)
 (("M-b" "s-b") . pyim-backward-word))

(provide 'init-pyim)
;;; init-pyim.el ends here
