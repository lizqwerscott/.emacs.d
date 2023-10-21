(require 'eshell)
(require 'project)

;;; eshell toggle
(defvar eshell-toggle--toggle-buffer-p nil)

(defun eshell-toggle--window-size ()
  "Width or height of the selected window, depends on `below'."
  (if (memq 'below '(left right))
      (window-text-width)
    (window-total-height)))

(defun eshell-toggle--split-window ()
  "Split window according to customization."
  (let* ((size (/ (eshell-toggle--window-size) 3))
         (count (if (memq 'below '(above left)) -1 1)))
    (split-window nil (- size) 'below)
    (other-window count)))

(defun eshell-toggle--new-buffer (buf-name)
  "Init BUF-NAME."
  (let ((default-directory (project-root (project-current t))))
    (eshell t))
  (rename-buffer buf-name)
  (setq eshell-toggle--toggle-buffer-p t))

;;;###autoload
(defun eshell-project-toggle ()
  "Show eshell at the bottom of current window and cd to current buffer's path.
(1) If eshell-toggle'd buffer is already visible in frame for
current buffer then select (toggled) eshell window.
(2) If current window is (toggled) eshell itself then hide it.
(3) If KEEP-VISIBLE is non-nil, (toggled) eshell window will stay
visible and will not be hidden."
  (interactive)
  (if (eq eshell-toggle--toggle-buffer-p t)
      ;; if selected window is eshell-toggle buffer itself just delete its window
      (progn
        ;; (delete-window)
        (setq eshell-toggle--toggle-buffer-p nil))
    (let ((buf-name (project-prefixed-buffer-name "eshell")))
      (if (get-buffer buf-name)
	      ;; buffer is already created
          (or (-some-> buf-name get-buffer-window delete-window)
	         (eshell-toggle--split-window)
             (switch-to-buffer buf-name))
        ;; buffer is not created, create it
        (eshell-toggle--split-window)
        (eshell-toggle--new-buffer buf-name)
        (switch-to-buffer buf-name)))))

;;; eshell prompt
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;; eshell completion
(when (and (executable-find "fish")
         (require 'fish-completion nil t))
  (global-fish-completion-mode))

(provide 'init-eshell)
