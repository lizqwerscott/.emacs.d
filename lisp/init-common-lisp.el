;;; init-common-lisp.el --- init common-lisp package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: processes

;;; Commentary:


;;; Code:

(use-package sly
  :ensure t
  :hook (sly-mode . (lambda ()
                      (unless (sly-connected-p)
                        (save-excursion (sly)))))
  :config
  ;; (setq sly-complete-symbol-function 'sly-simple-completions)
  (setq sly-complete-symbol-function 'sly-flex-completions)
  (setq inferior-lisp-program "sbcl"))

(defun sly-switch-mrepl (&optional display-action)
  "Find or create the first useful REPL for the default connection.
If supplied, DISPLAY-ACTION is called on the buffer.
Interactively, DISPLAY-ACTIOn defaults to using `switch-to-buffer'
unless the intended buffer is already visible in some window, in
which case that window is selected."
  (interactive
   (list (lambda (buf)
           (let ((w (get-buffer-window buf)))
             (if w (select-window w) (switch-to-buffer-other-window buf))))))
  (let* ((buffer (sly-mrepl--find-create (sly-current-connection))))
    (when display-action
      (funcall display-action buffer))
    buffer))

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sly-asdf
  :ensure t
  :after sly)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here.
