(require 'wucuo)

;; (setq wucuo-flyspell-start-mode "normal")
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq major-mode
                 '(dired-mode
                   log-edit-mode
                   compilation-mode
                   help-mode
                   profiler-report-mode
                   speedbar-mode
                   gud-mode
                   calc-mode
                   Info-mode)))))

(add-hooks '(prog-mode text-mode)
           #'(lambda ()
               (wucuo-start)))

(provide 'init-spell)
;;; init-spell.el ends here.
