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

(require 'aggressive-indent)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'c-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'go-ts-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'rust-ts-mode)
(add-hook 'emacs-lisp-mode-hook
          'aggressive-indent-mode)

(add-hook 'lisp-mode-hook
          'aggressive-indent-mode)

(add-hook 'python-mode-hook
          'aggressive-indent-mode)

;; (global-aggressive-indent-mode 1)

(require 'indent-yank)
(add-hook 'python-mode-hook 'indent-yank-mode)
(add-hook 'python-ts-mode-hook 'indent-yank-mode)

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

(provide 'init-edit)
;;; init-edit.el ends here.
