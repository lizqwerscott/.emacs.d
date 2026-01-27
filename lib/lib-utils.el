;;; lib-utils.el --- elisp utils                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defun convert-mode-to-hook (mode)
  "CONVERT MODE to hook."
  (let ((name (symbol-name mode)))
    (if (or (string-suffix-p "-hook" name)
            (string-suffix-p "-functions" name))
        mode
      (intern (concat name "-hook")))))

(defmacro setup-hooks (&rest modes-fns)
  "Add fn in modes.
MODES-FNS is a list of (modes . fn) pairs."
  `(progn
     ,@(apply #'append
              (mapcar (lambda (modes-fn)
                        (pcase-let* ((`(,modes . ,fn) modes-fn))
                          (mapcar (lambda (mode)
                                    `(add-hook (quote ,(convert-mode-to-hook mode)) (function ,fn)))
                                  (if (listp modes)
                                      modes
                                    (list modes)))))
                      modes-fns))))

(defmacro with-hook (modes &rest body)
  "Add lambda in MODES.
BODY is lambda body."
  (declare (indent 1))
  `(progn
     ,@(apply #'append
              (mapcar (lambda (mode)
                        `((add-hook (quote ,(convert-mode-to-hook mode))
                                    (function (lambda ()
                                                ,@body)))))
                      (if (listp modes)
                          modes
                        (list modes))))))

(defmacro add-hooks (modes fn)
  "Add FN in MODES hook."
  `(dolist (mode ,modes)
     (add-hook (convert-mode-to-hook mode)
               ,fn)))

(defun keymap--bind (key-bindings &optional keymap)
  "Set key in KEYMAP.
KEY-BINDINGS is a list of (KEYS . COMMAND) pairs, where KEYS can be a single key
or a list of keys, and COMMAND is the command to bind to those keys."
  (apply #'append
         (mapcar (lambda (key-binding)
                   (pcase-let* ((`(,keys . ,command) key-binding))
                     (mapcar (lambda (key)
                               `(define-key
                                 (or (if (and ,keymap (symbolp ,keymap))
                                         (symbol-value ,keymap) ,keymap)
                                     global-map)
                                 ,(if (vectorp key) key (read-kbd-macro key))
                                 ,(if (and (listp command)
                                           (not (equal (car command)
                                                       'lambda)))
                                      `(cons ,(car command) (function ,(cdr command)))
                                    `(function ,command))))
                             (if (listp keys)
                                 keys
                               (list keys)))))
                 key-bindings)))

(defmacro keymap-binds (keymaps &rest key-bindings)
  "Set keys in KEYMAPS.
KEY-MAPS is a list of keymap or a single keymap.
KEY-BINDINGS is a list of (KEYS . COMMAND) pairs, where KEYS can be a single key
or a list of keys, and COMMAND is the command to bind to those keys."
  (declare (indent 1))
  `(progn
     ,@(apply #'append
              (mapcar (lambda (keymap)
                        (keymap--bind key-bindings keymap))
                      (if (listp keymaps)
                          keymaps
                        (list keymaps))))))

(defmacro global-bind-keys (&rest key-bindings)
  "Set keys in global keymap.
KEY-BINDINGS is a list of (KEYS . COMMAND) pairs, where KEYS can be a single key
or a list of keys, and COMMAND is the command to bind to those keys."
  ;; (declare (indent 1))
  `(progn
     ,@(keymap--bind key-bindings)))

(defun add-subdirs-to-load-path (search-dir)
  "Add subdirectories of SEARCH-DIR to `load-path'.
This function recursively searches through SEARCH-DIR and adds any subdirectory
containing .el, .so, or .dll files to the `load-path'.

It filters out unnecessary directories to improve Emacs startup speed:
- Non-directory files
- Parent directories (\".\" and \"..\")
- Common build/version control directories
  (dist, node_modules, __pycache__, etc.)

The directories are added to the end of `load-path' to ensure proper loading
order from parent to child directories."
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录, 提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录, 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中, 提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t , 表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件, 顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path (file-truename subdir-path) t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

;;; ui
(eval-when-compile
  (require 'init-custom))

(defun +lizqwer/load-theme (new-theme)
  "Disable now theme, and load NEW-THEME."
  (unless (cl-find new-theme custom-enabled-themes)
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (ignore-errors
      (load-theme new-theme t nil))
    (unless user/show-modeline
      ;; Disable mode line.
      (set-face-attribute 'mode-line nil
                          :foreground "DarkRed"
                          :background "DarkRed"
                          :height 0.1
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "Gay10"
                          :background "Gay10"
                          :height 0.1
                          :box nil
                          :inherit 'unspecified))))

(defun +lizqwer/toggle-transparent ()
  "Toggle transparent."
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'alpha-background) 100)
      (set-frame-parameter (selected-frame) 'alpha-background 90)
    (set-frame-parameter (selected-frame) 'alpha-background 100)))

(defun +lizqwer/toggle-dark-theme ()
  "Toggle night and day theme."
  (interactive)
  (if (cl-find user/day-theme custom-enabled-themes)
      (+lizqwer/load-theme user/night-theme)
    (+lizqwer/load-theme user/day-theme)))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun lizqwer/api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host (or host)
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `api-key' found in the auth source")))

;;; Tools

(defun find-custom-file ()
  "Open custom file."
  (interactive)
  (find-file custom-file))

(defun find-init-file ()
  "Open `emacs' init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; From https://emacs.stackexchange.com/questions/5582/are-there-color-pickers-for-emacs
(defun my-insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

ARG is prefix argument.
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

(require 'comint)
(require 'shell)

;; from https://xenodium.com/emacs-clone-git-repo-from-clipboard
(defun ar/git-clone-clipboard-url (download-dir &optional depth)
  "Clone git URL in clipboard asynchronously and open in Dired when finished.
Then auto jump in README file.
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
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process _)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir)
                                         (goto-char (point-min))
                                         (when (re-search-forward "README" nil t)
                                           (beginning-of-line)))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(require 'battery)
(defun my/unsupport-battery-or-charging ()
  "Return t if battery status is unsupported or device is charging."
  (let* ((battery-infos (funcall battery-status-function))
         (power-supply-status (alist-get ?L battery-infos)))
    (or (string= power-supply-status "N/A")
        (string= power-supply-status "AC"))))

(provide 'lib-utils)
;;; lib-utils.el ends here
