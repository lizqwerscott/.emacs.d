;; (require 'sly)
(require 'sly-quicklisp)
;; (require 'sly-asdf)

(setq sly-complete-symbol-function 'sly-flex-completions)
;; (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
(setq inferior-lisp-program "ccl")

(require 'common-lisp-snippets)
(add-hook 'common-lisp-mode-hook
          #'common-lisp-snippets-initialize)

;;;###autoload
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

;;;###autoload
(defun project-name (project)
  "A human-readable name for the project.
Nominally unique, but not enforced."
  (file-name-nondirectory (directory-file-name (project-root project))))

;;;###autoload
(defun load-lisp-project ()
  "Load now project."
  (interactive)
  ;; (require 'sly)
  ;; (require 'sly-quicklisp)
  (sly-quickload (project-name (project-current))))

(keymap-sets sly-mode-map
             '(("C-c q r" . sly-restart-inferior-lisp)
               ("C-c q l" . load-lisp-project)))

(add-hook 'sly-mode-hook
          #'(lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

(add-hook 'lisp-mode-hook
          #'pmx-setup-check-parens)

(provide 'init-common-lisp)
