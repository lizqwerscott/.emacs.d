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
              `(("http" . ,(concat
                            user/proxy-host
                            ":20171"))
                ("https" . ,(concat
                             user/proxy-host
                             ":20171"))))
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

;;;##autoload
(defun +lizqwer/toggle-lock ()
  "Toggle computer lock."
  (interactive)
  (shell-command "screen-lock.sh"))

(provide 'init-func)
