;;; fussy-orderless.el --- fussy with orderless      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'cl-lib)

(require 'fussy)
(require 'orderless)

(require 'pyim)
(require 'flx-rs)

(defcustom fussy-orderless-affix-dispatch-alist '((?= . fussy-orderless-chinese-regexp-score)
                                                  (?! . fussy-orderless-not-score)
                                                  (?, . fussy-orderless-initialism-score))
  "A affix dispatch alist for score."
  :group 'fussy
  :type `(alist
          :key-type character
          :value-type (choice
                       (const :tag "Chinese Regexp" ,#'fussy-orderless-chinese-regexp-score)
                       (const :tag "Not" ,#'fussy-orderless-not-score)
                       (const :tag "Initialism" ,#'fussy-orderless-initialism-score)
                       (function :tag "Custom matching style"))))

(defun fussy-orderless--get-dispatch-key ()
  "Get dispatch key from orderless."
  (cl-remove-duplicates (mapcar (lambda (item) (char-to-string (car item)))
                                fussy-orderless-affix-dispatch-alist)))

(defun fussy-orderless--split-string-with-prefixs (query prefixs)
  "Split QUERY with PREFIXS."
  (let ((querys (split-string query))
        (prefix-items)
        (other-items)
        (index 0))
    (dolist (item querys)
      (let ((prefix (cl-find item prefixs :test (lambda (item prefix) (string-prefix-p prefix item)))))
        (if (and (stringp item)
                 (> (length item) 0)
                 prefix)
            (push (list (substring item (length prefix)) index prefix) prefix-items)
          (push (list item index) other-items)))
      (setq index (1+ index)))
    (list (reverse prefix-items) (reverse other-items) (length querys))))

(defun fussy-orderless-chinese-regexp-score (string query)
  "Use QUERY and STRING calc chinese regexp score."
  (when-let* (((string-match-p "\\cc" string))
              (regexp (pyim-cregexp-build query)))
    (string-match regexp string)
    (pcase-let* ((`(,start ,end) (match-data))
                 (len (length string)))
      (when (< end len)
        (list (+ (* 20 (/ (float (- end start))
                          len))
                 (* 80 (/ (float (- len start)) len)))
              start
              end)))))

(defun fussy-orderless-not-score (_ _)
  "Use QUERY and STRING calc score."
  '(0))

(defun fussy-orderless-initialism-score (string query)
  "Use QUERY and STRING calc score."
  (let* ((case-fold-search t)
         (string-len (length string))
         (start 0)
         (positions (catch 'fail
                      (mapcar
                       (lambda (c)
                         (let* ((re (format "\\_<%c" c))
                                (pos (string-match re string start)))
                           (unless pos (throw 'fail nil))
                           (setq start (1+ pos))
                           pos))
                       (string-to-list query)))))
    (when positions
      (let* ((pos-len (length positions))
             (score (round
                     (cl-reduce #'+
                                (cl-mapcar
                                 (lambda (pos idx)
                                   (* idx
                                      10
                                      (/ (- string-len pos)
                                         (float string-len))))
                                 positions
                                 (number-sequence 1 pos-len))))))
        (cons score
              (cl-mapcan (lambda (p) (list p (1+ p)))
                         positions))))))

(defun fussy-orderless-score (str prefix-items items-len)
  "Score STR for PREFIX-ITEMS using orderless.
ITEMS-LEN is all items length."
  (let* ((lst (mapcar (lambda (item)
                        (pcase-let* ((`(,query ,index ,prefix) item)
                                     (prefix (elt prefix 0))
                                     (fn (alist-get prefix fussy-orderless-affix-dispatch-alist)))
                          (when-let* ((fn)
                                      (res (funcall fn str query))
                                      (score (car res)))
                            (append (list
                                     (round (* (1+ (- items-len index))
                                               score)))
                                    (cdr res)))))
                      prefix-items))
         (filtered-lst (seq-filter 'identity lst)))
    (cons (apply #'+ (mapcar #'car filtered-lst))
          (apply #'append (mapcar #'cdr filtered-lst)))))

(defun fussy-orderless-score-with-flx-rs (str query &rest args)
  "Score STR for QUERY with ARGS using orderless."
  (pcase-let* ((keys (fussy-orderless--get-dispatch-key))
               (`(,prefix-items ,normal-items ,len) (fussy-orderless--split-string-with-prefixs query keys))
               (normal-query (string-join (mapcar #'car normal-items) ""))
               (prefix-scores (fussy-orderless-score str prefix-items len))
               (normal-scores (when (fboundp 'flx-rs-score)
                                (flx-rs-score (fussy-without-bad-char str) normal-query args))))
    (if normal-scores
        (if prefix-scores
            (append (list (+ (car prefix-scores) (car normal-scores)))
                    (sort (append (cdr normal-scores)
                                  (cdr prefix-scores))
                          '<))
          normal-scores)
      prefix-scores)))

(provide 'fussy-orderless)
;;; fussy-orderless.el ends here
