;;; init-hl-todo.el --- init hl todo                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq hl-todo-require-punctuation t)
(setq hl-todo-highlight-punctuation ":")

(setq hl-todo-keyword-faces
      '(("HOLD"   . "#d0bf8f")
        ("TODO"   . "#cc9393")
        ("NEXT"   . "#dca3a3")
        ("THEM"   . "#dc8cc3")
        ("PROG"   . "#7cb8bb")
        ("OKAY"   . "#7cb8bb")
        ("DONT"   . "#5f7f5f")
        ("FAIL"   . "#8c5353")
        ("DONE"   . "#afd8af")
        ("NOTE"   . "#d0bf8f")
        ("HACK"   . "#d0bf8f")
        ("TEMP"   . "#d0bf8f")
        ("FIXME"  . "#cc9393")))

(global-hl-todo-mode)

;;; consult todo

(with-eval-after-load 'consult-todo
  (require 'lib-hl-todo)
  (setq consult-todo-dir-function #'consult-todo--ripgrep))

(lazy-load-global-keys
 '(("M-g t" . consult-todo-project))
 "consult-todo")

(provide 'init-hl-todo)
;;; init-hl-todo.el ends here
