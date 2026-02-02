;;; init-helpful.el --- helpful                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lazy-load-global-keys
 '(("C-h f" . helpful-callable)
   ("C-h C-f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
 "helpful")

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(add-hooks '(help-mode helpful-mode)
           #'visual-line-mode)

(with-eval-after-load 'helpful
  (defun helpful-goto-info ()
    "View the *info* node of the current help item."
    (interactive)
    (info-lookup-symbol helpful--sym 'emacs-lisp-mode nil))

  (defun helpful-view-source ()
    "View the source of the current help item."
    (interactive)
    (pcase-let* ((primitive-p (helpful--primitive-p helpful--sym helpful--callable-p))
                 (lookfor-src-p (or (not primitive-p)
                                    find-function-C-source-directory))
                 (`(,buf ,pos _) (if lookfor-src-p
                                     (helpful--definition helpful--sym helpful--callable-p)
                                   '(nil nil nil))))
      (cond
       (buf
        (let ((path (buffer-file-name buf)))
          (if (not path)
              (switch-to-buffer-other-window buf)
            (find-file-other-window path)
            (revert-buffer-quick))
          (when pos
            (goto-char pos)
            (recenter))))
       (primitive-p
        (message "defined in C source code"))
       (t
        (message "without a source file")))))

  (defun helpful-customize ()
    "Customize variable or face whose doc string is shown in the current buffer."
    (interactive)
    (let ((sym helpful--sym))
      (unless (or (boundp sym) (facep sym))
        (user-error "No variable or face to customize"))
      (cond
       ((boundp sym) (customize-variable sym))
       ((facep sym) (customize-face sym)))))

  (defvar *helpful-buffer-ring-size 5
    "How many buffers are stored for use with `*helpful-next'.")

  (defvar *helpful--buffer-ring (make-ring *helpful-buffer-ring-size)
    "Ring that stores the current Helpful buffer history.")

  (defun *helpful--buffer-index (&optional buffer)
    "If BUFFER is a Helpful buffer, return it’s index in the buffer ring."
    (let ((buf (or buffer (current-buffer))))
      (and (eq (buffer-local-value 'major-mode buf) 'helpful-mode)
           (seq-position (ring-elements *helpful--buffer-ring) buf #'eq))))


  (defun *helpful--new-buffer-a (help-buf)
    (let ((buf-ring *helpful--buffer-ring))
      (let ((newer-buffers (or (*helpful--buffer-index) 0)))
        (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
      (when (/= (ring-size buf-ring) *helpful-buffer-ring-size)
        (ring-resize buf-ring *helpful-buffer-ring-size))
      (ring-insert buf-ring help-buf)))
  (advice-add #'helpful--buffer :filter-return #'*helpful--new-buffer-a)

  (defun *helpful--next (&optional buffer)
    "Return the next live Helpful buffer relative to BUFFER."
    (let ((buf-ring *helpful--buffer-ring)
          (index (or (*helpful--buffer-index buffer) -1)))
      (cl-block nil
        (while (> index 0)
          (cl-decf index)
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun *helpful--previous (&optional buffer)
    "Return the previous live Helpful buffer relative to BUFFER."
    (let ((buf-ring *helpful--buffer-ring)
          (index (1+ (or (*helpful--buffer-index buffer) -1))))
      (cl-block nil
        (while (< index (ring-length buf-ring))
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun *helpful-next ()
    "Go to the next Helpful buffer."
    (interactive)
    (when-let (buf (*helpful--next))
      (funcall helpful-switch-buffer-function buf)))

  (defun *helpful-previous ()
    "Go to the previous Helpful buffer."
    (interactive)
    (when-let (buf (*helpful--previous))
      (funcall helpful-switch-buffer-function buf)))

  (keymap-binds helpful-mode-map
    ("h" . meow-left)
    ("j" . meow-next)
    ("k" . meow-prev)
    ("l" . meow-right)
    ("d" . describe-mode)
    ("i" . helpful-goto-info)
    ("s" . helpful-view-source)
    ("c" . helpful-customize)
    ("b" . *helpful-previous)
    ("r" . *helpful-next)))

(keymap-binds help-mode-map
  ("h" . meow-left)
  ("j" . meow-next)
  ("k" . meow-prev)
  ("l" . meow-right)
  ("d" . describe-mode)
  ("n" . forward-button)
  ("p" . backward-button))

;;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-symbol-map
    ("h" . helpful-symbol)))

(provide 'init-helpful)
;;; init-helpful.el ends here
