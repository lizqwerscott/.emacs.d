(setq package-archives '(("gnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                         ("melpa" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
                         ("melpa-stable" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa-stable/")
                         ("org" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/org/")))

(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(setq package-check-signature t)

(require 'package)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-always-demond nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

(use-package use-package
  :ensure t
)

(require 'use-package)

(provide 'init-elpa)
