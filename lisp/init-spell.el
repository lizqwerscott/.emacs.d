;;; init-spell.el --- spell                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq jinx-languages "en_US")
(with-eval-after-load 'jinx
  (set-face-attribute 'jinx-misspelled nil :underline '(:color "OrangeRed") :weight 'bold)

  (add-list-to-list 'jinx-camel-modes
                    '(c++-ts-mode))
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  (setf (alist-get 'prog-mode jinx-include-faces)
        '(prog-mode font-lock-comment-face
                    font-lock-doc-face
                    ;; font-lock-string-face
                    ;; font-lock-function-name-face
                    ;; font-lock-variable-name-face
                    ))

  (add-to-list 'jinx-include-faces
               '(c++-ts-mode font-lock-function-name-face
                             font-lock-variable-name-face)))

(defun jinx-open-person-dict-dir ()
  "Open person dict Dir."
  (interactive)
  (find-file (file-truename "~/.config/enchant/")))

(add-hook 'emacs-startup-hook #'global-jinx-mode)

(global-set-keys
 '(("C-M-$" . jinx-languages)
   ("s-$" . jinx-correct)
   ("C-c d c" . jinx-correct)))

(provide 'init-spell)
;;; init-spell.el ends here.
