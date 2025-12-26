;;; init-completion.el --- init completion           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Only list the commands of the current modes
(when (boundp 'read-extended-command-predicate)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(setq completion-cycle-threshold 4
      completions-detailed t
      completion-auto-help nil
      completion-styles '(basic))

;;; fussy

(flx-rs-load-dyn)

(autoload #'fussy-orderless-score-with-flx-rs "fussy-orderless")

(with-eval-after-load 'fussy
  (add-to-list 'fussy-whitespace-ok-fns
               #'fussy-orderless-score-with-flx-rs))

(setopt fussy-score-fn 'fussy-orderless-score-with-flx-rs
        fussy-filter-fn 'fussy-filter-orderless-flex
        fussy-use-cache nil
        fussy-compare-same-score-fn 'fussy-histlen->strlen<)

(fussy-setup)
(fussy-eglot-setup)

(with-eval-after-load 'corfu
  ;; For cache functionality.
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-score-fn 'flx-rs-score
                          fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

;;; orderless
(with-eval-after-load 'orderless
  (add-to-list 'orderless-affix-dispatch-alist
               `(?& . ,#'orderless-literal)))
;; (setopt orderless-matching-styles nil)
;; (add-list-to-list 'completion-category-overrides
;;                   '((file (styles orderless fussy))
;;                     (project-file (styles orderless fussy))
;;                     (multi-category (styles orderless fussy basic))
;;                     (consult-location (styles orderless fussy basic))
;;                     (org-heading (styles orderless fussy basic))
;;                     (bookmark (styles orderless fussy basic))
;;                     (unicode-name (styles orderless fussy basic))))

;;; corfu
(require 'init-corfu)

;;; completion preview
(require 'init-completion-preview)

(provide 'init-completion)
;;; init-completion.el ends here
