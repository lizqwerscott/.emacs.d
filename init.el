
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)
            (setq gc-cons-percentage 0.6)))

(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name
              (concat user-emacs-directory "lisp")))

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
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path
 (concat user-emacs-directory
         "site-lisp/"))

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq custom-file "~/.config/emacs-custom.el")

(require 'init-const)
(require 'init-package)
(require 'init-startup)
(require 'init-tool)
(require 'init-edit)

(require 'init-program)
(require 'init-python)
(require 'init-c++)
(require 'init-web)
(require 'init-common-lisp)
(require 'init-rust)
(require 'init-sql)
(require 'init-go)

(require 'init-reader)
;; (require 'init-paper)

(require 'init-project)
(require 'keybinding)

(require 'init-org)

(require 'init-ui)
(require 'init-input)
;;(require 'crefactor)

(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here.
