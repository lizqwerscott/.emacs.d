(require 'vterm)
(require 'multi-vterm)

(setq multi-vterm-dedicated-window-height-percent 40)

(defun string-hash-case-sensitive (a)
  (sxhash a))
(defun string-equal-case-sensitive (a b)
  (string= a b))
(define-hash-table-test 'case-sensitive
                        'string-equal-case-sensitive
                        'string-hash-case-sensitive)

(defvar *multi-vterm-dir* (make-hash-table :test 'case-sensitive))

;;;###autoload
(defun multi-vterm-pop ()
  "Create new vterm buffer pop."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (switch-to-buffer-other-window vterm-buffer)))

;;;###autoload
(defun multi-vterm-open ()
  "Create new vterm buffer or open buffer"
  (interactive)
  (let ((res (gethash (file-truename default-directory)
                      *multi-vterm-dir*)))
    (if res
        (switch-to-buffer-other-window res)
      (let* ((vterm-buffer (multi-vterm-get-buffer)))
        (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
        (set-buffer vterm-buffer)
        (multi-vterm-internal)
        (setf (gethash (file-truename default-directory)
                       *multi-vterm-dir*)
              vterm-buffer)
        (switch-to-buffer-other-window vterm-buffer)))))

;;;###autoload
(cl-defun multi-vterm-run (run-command)
  "Open dedicated `multi-vterm' window."
  (interactive)
  (autoload 'project-root-path "init-project" nil t)
  (if (project-root-path)
      (call-interactively #'multi-vterm-project)
    (call-interactively #'multi-vterm-dedicated-toggle))
  (vterm-send-M-w)
  (vterm-send-string run-command t)
  (vterm-send-return))

(provide 'init-vterm)
