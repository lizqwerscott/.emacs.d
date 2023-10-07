;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)

(electric-pair-mode)

;; (use-package lispy
;;   :ensure t
;;   :hook (lisp-mode . lispy-mode))

(require 'init-awesome-pair)


(add-hooks '(emacs-lisp-mode lisp-mode python-mode)
           #'(lambda ()
               (require 'aggressive-indent)
               (aggressive-indent-mode 1)))

(add-hooks '(python-mode python-ts-mode)
           #'(lambda ()
               (require 'indent-yank)
               (indent-yank-mode 1)))

;; auto mark comment
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
      (if (and (third pre-comment-pos)
             (third next-comment-pos))
          (progn
            (setq start-comment-pos
                  (second pre-comment-pos))
            (setq end-comment-pos
                  (second next-comment-pos)))
        (progn
          (if (third pre-comment-pos)
              (progn
                (setq start-comment-pos
                      (second pre-comment-pos))
                (setq end-comment-pos
                      (first pre-comment-pos)))
            (if (third next-comment-pos)
                (progn
                  (setq start-comment-pos
                        (first next-comment-pos))
                  (setq end-comment-pos
                        (second next-comment-pos)))))))
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

      (if (and (second pre-comment-pos)
             (first pre-comment-pos))
          (progn
            (set-mark (first pre-comment-pos))
            (goto-char (second pre-comment-pos)))
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

      (if (and (second pre-comment-pos)
             (first pre-comment-pos))
          (progn
            (set-mark (second pre-comment-pos))
            (goto-char (first pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(grugru-default-setup)

(provide 'init-edit)
;;; init-edit.el ends here.
