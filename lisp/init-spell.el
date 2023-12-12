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

;; (use-package jinx
;;   :ensure t
;;   :hook ((org-mode . jinx-mode))
;;   :bind ([remap ispell-word] . jinx-correct)
;;   :config
;;   (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
;;   (vertico-multiform-mode 1)
;;   (add-to-list 'vertico-multiform-categories
;;                '(jinx grid (vertico-grid-annotate . 25))))


(provide 'init-spell)
;;; init-spell.el ends here.
