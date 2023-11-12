(require 'posframe)

(defun pyim-probe-meow-normal-mode ()
  "probe meow normal mode"
  (symbol-value 'meow-normal-mode))

(require 'pyim)
(setq default-input-method "pyim")
(pyim-default-scheme 'quanpin)
;; (setq pyim-cloudim 'baidu)
(setq pyim-page-tooltip 'posframe)
(setq-default pyim-english-input-switch-functions
              `(
                ;; pyim-probe-program-mode
                pyim-probe-meow-normal-mode
                pyim-probe-org-structure-template))
(setq-default pyim-punctuation-translate-p '(no))

;; (require 'pyim-basedict)
;; (pyim-basedict-enable)
;; (require 'pyim-tsinghua-dict)
;; (pyim-tsinghua-dict-enable)

(global-set-key (kbd "C-\\") 'toggle-input-method)

(defun my-orderless-regexp (orig-func component)
  (let ((result (funcall orig-func component)))
    (pyim-cregexp-build result)))

(advice-add 'orderless-regexp :around #'my-orderless-regexp)

(require 'pyim-cstring-utils)
(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)

(provide 'init-input)
