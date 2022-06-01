(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
             t)

(package-initialize)

(provide 'init-melpa)
