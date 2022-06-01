
(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)

(setq default-input-method "pyim")
(setq pyim-cloduim 'baidu)
(global-set-key (kbd "C-i") 'toggle-input-method)


(provide 'init-input)
