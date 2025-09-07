;;; init-utils.el --- utils                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;;;###autoload
(defun mapcar-if (fn seq handle)
  (mapcar #'(lambda (v)
              (if (funcall handle v)
                  (funcall fn v)
                v))
          seq))

;;;###autoload
(defun mapcar-if-not (fn seq handle)
  (mapcar #'(lambda (v)
              (if (funcall handle v)
                  v
                (funcall fn v)))
          seq))

;;;###autoload
(defmacro add-hooks (modes fn)
  `(dolist (mode ,modes)
     (add-hook (intern
                (concat (symbol-name mode)
                        "-hook"))
               ,fn)))

;;;###autoload
(defmacro keymap-sets (key-maps key-bindings)
  "Set keys in keymaps.
KEY-MAPS is a list of keymaps.
KEY-BINDINGS is a list of (KEYS . COMMAND) pairs, where KEYS can be a single key
or a list of keys, and COMMAND is the command to bind to those keys."
  (declare (indent 1))
  `(dolist (key-map ,(if (listp key-maps)
                         `(list ,@key-maps)
                       `(list ,key-maps)))
     (dolist (key-b ,key-bindings)
       (when-let* ((keys (car key-b))
                   (command (cdr key-b)))
         (if (listp keys)
             (dolist (key keys)
               (keymap-set key-map
                           key
                           command))
           (keymap-set key-map
                       keys
                       command))))))

;;;###autoload
(defmacro global-set-keys (key-bindings)
  "Set keys in global keymap.
KEY-BINDINGS is a list of (KEYS . COMMAND) pairs, where KEYS can be a single key
or a list of keys, and COMMAND is the command to bind to those keys."
  ;; (declare (indent 1))
  `(dolist (key-b ,key-bindings)
     (when-let* ((keys (car key-b))
                 (command (cdr key-b)))
       (if (listp keys)
           (dolist (key keys)
             (keymap-global-set key command))
         (keymap-global-set keys command)))))

;;;###autoload
(defun add-list-to-list (list-var elements)
  (if (listp elements)
      (mapcar #'(lambda (element)
                  (add-to-list list-var element))
              (reverse elements))
    (add-to-list list-var elements)))

(defun add-subdirs-to-load-path (search-dir)
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

;;;###autoload
(defun +lizqwer/load-theme (new-theme)
  "Load theme."
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

;;; cons to list
(defun single-cons-p (c)
  "判断 C 是否为单个 cons 对象（cdr 不是 cons 且不为 nil）。"
  (and (consp c)
       (not (consp (cdr c)))
       (not (null (cdr c)))))

(defun cons-to-list-s (s)
  (if (single-cons-p s)
      (list (car s) (cdr s))
    (cons-to-list s)))

;;;###autoload
(defun cons-to-list (c)
  "将 cons 对象递归地转换为列表。"
  (when c
    (if (listp c)
        (cons (cons-to-list-s (car c))
              (cons-to-list-s (cdr c)))
      c)))

;;;###autoload
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
        emacs-basic-display
        (not (display-graphic-p)))))

;;;###autoload
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

;;;###autoload
(defun find-custom-file()
  "Open custom files."
  (interactive)
  (find-file custom-file))

(provide 'init-utils)
;;; init-utils.el ends here
