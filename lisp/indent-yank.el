(require 'dash)

(defun indent-yank-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "\\_>*$")))

(defun indent-yank-remove-indent-in-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((indent most-positive-fixnum))
      (while (not (eobp))
        (when (not (indent-yank-line-empty-p))
          (setq indent (min indent (current-indentation))))
        (forward-line 1))
      (goto-char (point-min))
      (while (not (eobp))
        (when (>= (current-indentation) indent)
          (beginning-of-line)
          (delete-forward-char indent))
        (forward-line 1)))
    (buffer-string)))

(defun indent-yank-yank (&optional arg)
  (interactive "*P")
  (let* ((arg (or arg 1))
         (indent (current-indentation))
         (text-without-indent (indent-yank-remove-indent-in-string (current-kill 0)))
         (text-yank (replace-regexp-in-string
                     "\n" (concat "\n" (-repeat indent ? )) text-without-indent)))
    (let ((head (nth (- arg 1) kill-ring)))
      (setf (nth (- arg 1) kill-ring) text-yank)
      (yank arg)
      (setf (car kill-ring) head))))

(defun indent-yank-before-insert-for-yank (args)
  (if indent-yank-mode (list (replace-regexp-in-string "\n" (concat "\n" (-repeat (current-indentation) ? )) (indent-yank-remove-indent-in-string (car args))))
    args))

(advice-add #'insert-for-yank :filter-args #'indent-yank-before-insert-for-yank)

(define-minor-mode indent-yank-mode
  "Minor mode for yanking based on indentation at point.")

(provide 'indent-yank)
