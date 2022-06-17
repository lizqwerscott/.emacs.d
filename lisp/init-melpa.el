(require 'package)

;; (add-to-list 'package-archives
;;              '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;              t)

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'init-melpa)
