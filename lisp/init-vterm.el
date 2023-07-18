(require 'vterm)
(require 'vterm-toggle)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                 (let ((buffer (get-buffer buffer-or-name)))
                   (with-current-buffer buffer
                     (or (equal major-mode 'vterm-mode)
                        (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-at-bottom)
               ;;(display-buffer-reuse-window display-buffer-in-direction)
               ;;display-buffer-in-direction/direction/dedicated is added in emacs27
               ;;(direction . bottom)
               ;;(dedicated . t) ;dedicated is supported in emacs27
               (reusable-frames . visible)
               (window-height . 0.3)))

(defvar vterm-run-buffer nil)

;;;###autoload
(defun vterm-run (run-command)
  "Run the program including the current buffer in `vterm'."
  (interactive)
  (let* ((w (vterm-toggle--get-window)))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-run-buffer (current-buffer))
        (rename-buffer "*vterm run*")
        (compilation-shell-minor-mode 1)
        (vterm-send-M-w)
        (vterm-send-string run-command t)
        (vterm-send-return)))))

(defvar vterm-compile-buffer nil)

;;;###autoload
(defun vterm-compile ()
  "Compile the program including the current buffer in `vterm'."
  (interactive)
  (let* ((command (eval compile-command))
         (w (vterm-toggle--get-window)))
    (setq-local compile-command (compilation-read-command command))
    (setf command compile-command)
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (let ((root (project-root (project-current))))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (when root
            (vterm-send-string (concat "cd "
                                       root)
                               t)
            (vterm-send-return))
          (vterm-send-string command t)
          (vterm-send-return))))))

(add-hook 'vterm-mode-hook
          #'(lambda ()
              (meow-insert-mode)))

(add-hook 'meow-normal-mode-hook
          #'(lambda ()
              (if (equal major-mode #'vterm-mode)
                  (vterm-copy-mode))))

(add-hook 'meow-insert-mode-hook
          #'(lambda ()
              (if (equal major-mode #'vterm-mode)
                  (vterm-copy-mode -1))))

(provide 'init-vterm)
