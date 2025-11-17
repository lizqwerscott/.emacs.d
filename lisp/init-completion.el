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
      completion-styles '(basic))

;;; fussy

(flx-rs-load-dyn)

(defun split-string-with-prefix (query prefix)
  "Split QUERY with PREFIX."
  (let ((querys (split-string query))
        (prefix-items)
        (other-items)
        (index 0))
    (dolist (item querys)
      (if (and (stringp item)
               (> (length item) 0)
               (string= (substring item 0 1) prefix))
          (push (list (substring item 1) index) prefix-items)
        (push (list item index) other-items))
      (setq index (1+ index)))
    (list (reverse prefix-items) (reverse other-items))))

(defun calc-chinese-score (query string)
  "Use QUERY and STRING calc score."
  (when-let* ((regexp (pyim-cregexp-build query)))
    (string-match regexp string)
    (pcase-let* ((`(,start ,end) (match-data))
                 (len (length string)))
      (when (< end len)
        (list (+ (* 20 (/ (float (- end start))
                          len))
                 (* 8000 (/ (float start) len)))
              start
              end)))))

(defun fussy-flx-rs-score-with-chinese (str query &rest args)
  "Score STR for QUERY with ARGS using `flx-rs-score'."
  (require 'flx-rs)
  (pcase-let* ((`(,prefix-items ,other-items) (split-string-with-prefix (string-trim query) "="))
               (other-query (string-join (mapcar #'car other-items) ""))
               (flx-score (when (fboundp 'flx-rs-score)
                            (flx-rs-score (fussy-without-bad-char str) other-query args)))
               (prefix-score 0)
               (prefix-item-pos))
    (when (string-match-p "\\cc" str)
      (dolist (item prefix-items)
        (pcase-let* ((`(,query ,index) item)
                     (score-pos (calc-chinese-score query str))
                     (all-len (+ (length prefix-items)
                                 (length other-items))))
          (when score-pos
            (cl-incf prefix-score
                     (* (float (1+ (- all-len index)))
                        (car score-pos)))
            (setq prefix-item-pos
                  (append prefix-item-pos
                          (cdr score-pos))))))
      (setq prefix-score (round prefix-score)))
    (if flx-score
        (append (list (+ prefix-score (car flx-score)))
                (sort (append (cdr flx-score)
                              prefix-item-pos)
                      '<))
      (unless other-items
        (unless (= prefix-score 0)
          (append (list prefix-score) prefix-item-pos))))))

(with-eval-after-load 'fussy
  (add-to-list 'fussy-whitespace-ok-fns
               #'fussy-flx-rs-score-with-chinese))

(setopt fussy-score-fn 'fussy-flx-rs-score-with-chinese
        fussy-filter-fn 'fussy-filter-orderless-flex
        fussy-use-cache t
        fussy-compare-same-score-fn 'fussy-histlen->strlen<)

(fussy-setup)
(fussy-eglot-setup)

(with-eval-after-load 'corfu
  ;; For cache functionality.
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

;;; orderless
(setopt orderless-matching-styles nil)
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
