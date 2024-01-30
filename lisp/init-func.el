;;; Look require cl package
;;;###autoload
(defun +lizqwer/check-package-use-cl ()
  "Check package is use cl."
  (interactive)
  (require 'loadhist)
  (file-dependents (feature-file 'cl)))

;;;###autoload
(defun +lizqwer/toggle-transparent ()
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'alpha-background) 100)
      (set-frame-parameter (selected-frame) 'alpha-background 90)
    (set-frame-parameter (selected-frame) 'alpha-background 100)))

;;;###autoload
(defun +lizqwer/toggle-proxy ()
  (interactive)
  (if (null url-proxy-services)
      (progn
        (setq url-proxy-services
              (get-url-proxy))
        (message "代理已开启."))
    (setq url-proxy-services nil)
    (message "代理已关闭.")))

;;some tool function

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun tianqi ()
  "获取天气."
  (interactive)
  (eww "zh-cn.wttr.in/"))

(defun get-file-path ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

;;;###autoload
(defun +lizqwer/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (car (last (file-name-split (get-file-path))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun +lizqwer/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (get-file-path)))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

;;;###autoload
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;;###autoload
(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun find-custom-file()
  "Open custom files."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p centaur-custom-example-file)
        (copy-file centaur-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" centaur-custom-example-file)))
  (when (file-exists-p custom-file)
    (find-file custom-file))
  (when (file-exists-p centaur-custom-post-file)
    (find-file-other-window centaur-custom-post-file)))

;;;###autoload
(defun +lizqwer/toggle-lock ()
  "Toggle computer lock."
  (interactive)
  (shell-command "screen-lock.sh"))

;;;###autoload
(defun +lizqwer/toggle-dark-theme ()
  "Toggle theme."
  (interactive)
  (if (eq user/day-theme user/now-theme)
      (+lizqwer/load-theme user/night-theme)
    (+lizqwer/load-theme user/day-theme)))

;; From https://emacs.stackexchange.com/questions/5582/are-there-color-pickers-for-emacs
;;;###autoload
(defun my-insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

With prefix argument \\[universal-argument] insert the 48-bit value."
  (interactive "*P")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
            (interactive)
            (quit-window)
            (with-current-buffer ,buf
              (insert (apply #'color-rgb-to-hex
                             (nconc (color-name-to-rgb name)
                                    (unless (consp ',arg)
                                      (list (or ,arg 2)))))))))))

;;;###autoload
(defun insert-import ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (goto-char (point-min)))

(provide 'init-func)
