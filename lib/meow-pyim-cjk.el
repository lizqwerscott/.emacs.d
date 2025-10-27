;;; meow-pyim-cjk.el --- meow pyim cjk               -*- lexical-binding: t; -*-

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

(require 'pyim-cstring-utils)
(require 'meow)

(defun meow-mark-thing-cjk (thing type &optional backward regexp-format)
  "Make expandable selection of THING, with TYPE and forward/BACKWARD direction.

THING is a symbol usable by `forward-thing', which see.

TYPE is a symbol. Usual values are `word' or `line'.

The selection will be made in the \\='forward\\=' direction unless BACKWARD is
non-nil.

When REGEXP-FORMAT is non-nil and a string, the content of the selection will be
quoted to regexp, then pushed into `regexp-search-ring' which will be read by
`meow-search' and other commands. In this case, REGEXP-FORMAT is used as a
format-string to format the regexp-quoted selection content (which is passed as
a string to `format'). Further matches of this formatted search will be
highlighted in the buffer."
  (interactive "p")
  (let ((direction (if backward 'backward 'forward)))
    (if (or (eq type 'symbol) (not (looking-at-p "\\cc")))
        (meow--select-noncjk thing type backward regexp-format)
      (meow--select-cjk direction backward))))

(defun meow--select-noncjk (thing type backward regexp-format)
  "Select non-CJK text based on THING, TYPE, and BACKWARD direction."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (meow--make-selection (cons 'expand type) beg end)
        (meow--select t backward))
      (when (stringp regexp-format)
        (let ((search (format regexp-format (regexp-quote (buffer-substring-no-properties beg end)))))
          (meow--push-search search)
          (meow--highlight-regexp-in-buffer search))))))

(defun meow--select-cjk (direction backward)
  "Select CJK text based on DIRECTION and BACKWARD direction."
  (when-let* ((point-pos (point))
              (segments (pyim-cstring-words-at-point (equal direction 'forward)))
              (bounds (cdar segments))
              (segment-bounds (cons (elt bounds 0)
                                    (elt bounds 1))))
    (when-let* ((seg-beg (- point-pos (car segment-bounds)))
                (seg-end (+ point-pos (cdr segment-bounds)))
                (segment-text (buffer-substring-no-properties seg-beg seg-end))
                (regexp (regexp-quote segment-text)))
      (let ((selection (meow--make-selection (cons 'expand 'word) seg-beg seg-end)))
        (meow--select selection t backward)
        (meow--push-search regexp)
        (meow--highlight-regexp-in-buffer regexp)))))

(defun meow-next-thing-cjk (thing type n &optional include-syntax)
  "Create non-expandable selection of TYPE to the end of the next Nth THING.

If N is negative, select to the beginning of the previous Nth thing instead."
  (unless (equal type (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (unless include-syntax
    (setq include-syntax
          (let ((thing-include-syntax
                 (or (alist-get thing meow-next-thing-include-syntax)
                     '("" ""))))
            (if (> n 0)
                (car thing-include-syntax)
              (cadr thing-include-syntax)))))
  (let* ((expand (equal (cons 'expand type) (meow--selection-type)))
         (_ (when expand
              (if (< n 0) (meow--direction-backward)
                (meow--direction-forward))))
         (new-type (if expand (cons 'expand type) (cons 'select type)))
         (m (point))
         (p (save-mark-and-excursion
              (if (and (eq thing 'word)
                       (not (equal (buffer-substring-no-properties m (+ m 1))
                                   " ")))
                  (progn
                    (if (> n 0)
                        (pyim-forward-word n)
                      (pyim-backward-word (- n))))
                (forward-thing thing n))
              (unless (= (point) m)
                (point)))))
    (when p
      (thread-first
        (meow--make-selection
         new-type
         (meow--fix-thing-selection-mark thing p m include-syntax)
         p
         expand)
        (meow--select t))
      (meow--maybe-highlight-num-positions
       (cons (apply-partially #'meow--backward-thing-1-cjk thing)
             (apply-partially #'meow--forward-thing-1-cjk thing))))))

(defun meow--forward-thing-1-cjk (thing)
  (let ((pos (point)))
    (if (and (eq thing 'word)
             (not (equal (buffer-substring-no-properties pos (+ pos 1))
                         " ")))
        (pyim-forward-word 1)
      (forward-thing thing 1))
    (when (not (= pos (point)))
      (meow--hack-cursor-pos (point)))))

(defun meow--backward-thing-1-cjk (thing)
  (let ((pos (point)))
    (if (and (eq thing 'word)
             (not (equal (buffer-substring-no-properties (- pos 1) pos)
                         " ")))
        (pyim-backward-word 1)
      (forward-thing thing -1))
    (when (not (= pos (point)))
      (point))))

(provide 'meow-pyim-cjk)
;;; meow-pyim-cjk.el ends here
