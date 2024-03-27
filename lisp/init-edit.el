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

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(require 'init-awesome-pair)

(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'aggressive-indent)
               (aggressive-indent-mode 1)))

(add-hooks '(python-mode python-ts-mode)
           #'(lambda ()
               (require 'indent-yank)
               (indent-yank-mode 1)))

;;; Auto rename tag
(add-hooks '(html-mode web-mode)
           #'auto-rename-tag-mode)

;;; auto mark comment
;; from https://github.com/magnars/expand-region.el/blob/b70feaa644310dc2d599dc277cd20a1f2b6446ac/er-basic-expansions.el#L102
(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
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
  (save-excursion
    (back-to-indentation)
    (or (memq (get-text-property (point) 'face)
             '(font-lock-comment-face font-lock-comment-delimiter-face web-mode-comment-face)))))

(defun line-empty-p ()
  (string= (s-trim
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))
           ""))

;; cant mark top buffer comment
(defun find-comment-area (forwardp)
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
          (previous-line)
        (next-line)))
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
          (end-comment-pos nil))
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
    (let ((p (point)))
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
  "Mark next line comment"
  (interactive)
  (when (or (line-comment-p)
           (line-empty-p))
    (let ((p (point)))
      (setq pre-comment-pos (find-comment-area nil))

      (if (and (cl-second pre-comment-pos)
             (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-second pre-comment-pos))
            (goto-char (cl-first pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

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

(grugru-default-setup)

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

;;; insert something
(defun +lizqwer/insert-file-path (filename)
  (interactive "*fInsert file path: \n")
  (insert (expand-file-name filename)))

(defun +lizqwer/insert-file-path-abbrev (filename)
  (interactive "*fInsert file path: \n")
  (insert (abbreviate-file-name (expand-file-name filename))))

(defun +lizqwer/insert-file-path-relative (filename)
  (interactive "*fInsert file path: \n")
  (insert (file-relative-name filename)))

(defun +lizqwer/insert-file-name (filename)
  (interactive "fInsert file name: \n")
  (insert (file-name-nondirectory filename)))

(one-key-create-menu
 "Insert-file-path"
 '((("f" . "insert full path") . +lizqwer/insert-file-path)
   (("r" . "insert relative path") . +lizqwer/insert-file-path-relative)
   (("a" . "insert abbrev path") . +lizqwer/insert-file-path-abbrev)))

(one-key-create-menu
 "Insert-file"
 '((("p" . "insert file path") . one-key-menu-insert-file-path)
   (("n" . "insert file name") . +lizqwer/insert-file-name)))

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
