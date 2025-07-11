;;; mark-comment.el --- search and mark comment      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

;;; auto mark comment
;; from https://github.com/magnars/expand-region.el/blob/b70feaa644310dc2d599dc277cd20a1f2b6446ac/er-basic-expansions.el#L102
(defun er--point-is-in-comment-p ()
  "Determine whether the current cursor position is in a comment.
t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er/mark-comment ()
  "Mark the entire comment around point."
  (interactive)
  (when (er--point-is-in-comment-p)
    (let ((p (point)))
      (while (and (er--point-is-in-comment-p) (not (eobp)))
        (forward-char 1))
      (skip-chars-backward "\n\r")
      (set-mark (point))
      (goto-char p)
      (while (er--point-is-in-comment-p)
        (forward-char -1))
      (forward-char 1))))

(defun line-comment-p ()
  "Determine whether the current cursor position is in a comment."
  (save-excursion
    (back-to-indentation)
    (or (memq (get-text-property (point) 'face)
              '(font-lock-comment-face font-lock-comment-delimiter-face web-mode-comment-face)))))

(defun line-empty-p ()
  "Determine whether the current line is empty."
  (string= (string-trim
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))
           ""))

(defun find-comment-area (forwardp)
  "Find comment area from point.
FORWARDP If it is t, search upward; if it is NIL, search downward."
  (let ((findp t)
        (first-find t)
        (first-comment-pos nil)
        (last-comment-pos nil))
    (while (and findp
                (not (if forwardp
                         (= (line-beginning-position) (point-min))
                       (= (line-end-position) (point-max)))))
      (if (line-comment-p)
          (progn
            (when first-find
              (setf first-find nil)
              (setf first-comment-pos
                    (if forwardp
                        (line-end-position)
                      (line-beginning-position))))
            (setf last-comment-pos
                  (if forwardp
                      (line-beginning-position)
                    (line-end-position))))
        (when (not (line-empty-p))
          (setf findp nil)))
      (if forwardp
          (forward-line -1)
        (forward-line)))
    (when (if forwardp
              (= (line-beginning-position) (point-min))
            (= (line-end-position) (point-max)))
      (when (line-comment-p)
        (if forwardp
            (setf last-comment-pos (point-min))
          (setf first-comment-pos (point-max)))))
    (list first-comment-pos
          last-comment-pos
          (and first-comment-pos
               last-comment-pos))))

(defun lizqwer/mark-line-comment ()
  "Mark the line comment around point."
  (interactive)
  (when (or (line-comment-p)
            (line-empty-p))
    (let ((p (point))
          (start-comment-pos nil)
          (end-comment-pos nil)
          (pre-comment-pos)
          (next-comment-pos))
      (setq pre-comment-pos (find-comment-area t))
      (goto-char p)
      (setq next-comment-pos (find-comment-area nil))
      (if (and (cl-third pre-comment-pos)
               (cl-third next-comment-pos))
          (progn
            (setq start-comment-pos
                  (cl-second pre-comment-pos))
            (setq end-comment-pos
                  (cl-second next-comment-pos)))
        (progn
          (if (cl-third pre-comment-pos)
              (progn
                (setq start-comment-pos
                      (cl-second pre-comment-pos))
                (setq end-comment-pos
                      (cl-first pre-comment-pos)))
            (if (cl-third next-comment-pos)
                (progn
                  (setq start-comment-pos
                        (cl-first next-comment-pos))
                  (setq end-comment-pos
                        (cl-second next-comment-pos)))))))
      (if (and start-comment-pos
               end-comment-pos)
          (progn
            (goto-char end-comment-pos)
            (set-mark end-comment-pos)
            (goto-char start-comment-pos))
        (message "not find comment!")))))

(defun mark-pre-comment ()
  "Mark pre line comment."
  (interactive)
  (when (or (line-comment-p)
            (line-empty-p))
    (let ((p (point))
          (pre-comment-pos))
      (setq pre-comment-pos (find-comment-area t))

      (if (and (cl-second pre-comment-pos)
               (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-first pre-comment-pos))
            (goto-char (cl-second pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

(defun mark-next-comment ()
  "Mark next line comment."
  (interactive)
  (when (or (line-comment-p)
            (line-empty-p))
    (let ((p (point))
          (pre-comment-pos))
      (setq pre-comment-pos (find-comment-area nil))

      (if (and (cl-second pre-comment-pos)
               (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-second pre-comment-pos))
            (goto-char (cl-first pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

(defun mark-comment-inner-of-comment ()
  "Get comment start and end pos."
  (when (or (line-comment-p)
            (line-empty-p))
    (let* ((pre-comment-pos)
           (next-comment-pos))
      (save-excursion
        (setq pre-comment-pos (find-comment-area t)))
      (save-excursion
        (setq next-comment-pos (find-comment-area nil)))
      (when-let* ((start (or (cl-second pre-comment-pos)
                             (cl-first next-comment-pos)))
                  (end (or (cl-second next-comment-pos)
                           (cl-first pre-comment-pos))))
        (cons start end)))))

(provide 'mark-comment)
;;; mark-comment.el ends here
