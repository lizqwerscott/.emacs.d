;;; init-func.el --- init some functions             -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun +lizqwer/toggle-dark-theme ()
  "Toggle theme."
  (interactive)
  (if (cl-find user/day-theme custom-enabled-themes)
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
(defun consult-fd-dir ()
  (interactive)
  (let ((consult-fd-args (append consult-fd-args
                                 (list
                                  "--type directory"))))
    (consult-fd "~/")))

;;;###autoload
;; from https://xenodium.com/emacs-clone-git-repo-from-clipboard
(defun ar/git-clone-clipboard-url (download-dir &optional depth)
  "Clone git URL in clipboard asynchronously and open in Dired when finished.
When called interactively, prompts for DOWNLOAD-DIR with ~/github as default.
With DEPTH, clone with --depth=1."
  (interactive (list (read-directory-name "Download directory: " "~/github/" nil t)
                     current-prefix-arg))
  (cl-assert (or (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0))
                 (string-match-p "^git@.*:" (current-kill 0)))
             nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (depth-arg (if depth "--depth=1" ""))
         (command (format "git clone %s %s" depth-arg url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(provide 'init-func)
;;; init-func.el ends here
