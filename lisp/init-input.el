(require 'posframe)

(defun pyim-probe-meow-normal-mode ()
  "probe meow normal mode"
  (symbol-value 'meow-normal-mode))

(require 'pyim)
(setq default-input-method "pyim")
(setq pyim-cloudim 'baidu)
(setq pyim-page-tooltip 'posframe)
(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq-default pyim-english-input-switch-functions
              `(
                ;; pyim-probe-program-mode
                pyim-probe-meow-normal-mode
                pyim-probe-org-structure-template))
(setq-default pyim-punctuation-translate-p '(no))

(require 'pyim-basedict)
(pyim-basedict-enable)

(provide 'init-input)
