;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
  ;; ARGS               -> '((REV AUTHOR DATE))
  ;; (car ARGS)         -> '(REV AUTHOR DATE)
  ;; (nth 1 (car ARGS)) -> AUTHOR
  (let* ((author (nth 1 (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))

(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (setq magit-delta-hide-plus-minus-markers nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(with-eval-after-load 'magit-log
  ;; Set `magit-log-margin' value in :init as many other variables will be
  ;; dynamically set based on its value when `magit-log' is loaded.
  ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
  ;; Show the commit ages with 1-char time units
  ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
  ;; Also reduce the author column width to 11 as the author name is being
  ;; abbreviated below.
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
  (advice-add 'magit-log-format-margin :filter-args #'+magit-log--abbreviate-author))

(add-hook 'magit-mode-hook
          #'(lambda ()
              (magit-wip-mode t)
              (magit-delta-mode t)))

(with-eval-after-load 'magit
  (require 'forge))

(defun unpackaged/open-magit-status (status-fn)
  "Use STATUS-FN Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively status-fn)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-status))

;;;###autoload
(defun unpackaged/magit-project-status ()
  "Open a `magit-project-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-project-status))

(provide 'init-magit)
