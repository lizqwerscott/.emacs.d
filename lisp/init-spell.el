(require 'jinx)
(keymap-global-set "C-M-$" #'jinx-correct)
(add-to-list 'jinx-exclude-regexps '(t "\\cc"))
(add-hooks '(prog-mode text-mode)
           #'jinx-mode)

(provide 'init-spell)
;;; init-spell.el ends here.
