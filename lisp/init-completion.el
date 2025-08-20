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

(setq completion-cycle-threshold 4)

;;; orderless
(require 'orderless)
(setq completion-styles '(orderless fussy basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      orderless-component-separator #'orderless-escapable-split-on-space)

;;; fussy
;; flx-rs
(require 'flx-rs)
(setq fussy-score-fn 'fussy-flx-rs-score)
(flx-rs-load-dyn)

;;; corfu
(require 'init-corfu)

;;; completion preview
(require 'init-completion-preview)

(provide 'init-completion)
;;; init-completion.el ends here
