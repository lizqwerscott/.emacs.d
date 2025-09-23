;;; lib-hl-todo.el --- hl-todo                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tool

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'hl-todo)
(require 'consult-todo)
(require 'rg)

;; or convert from hl-todo--regexp (replace-regexp-in-string "\\\\[_<>]*" "" (with-temp-buffer (hl-todo--regexp t)))
(defun my/hl-todo-generate-ripgrep-regexp ()
  "Generate a regexp string for all TODO keywords.
The regexp will match any of the keywords defined in `hl-todo-keyword-faces',
surrounded by word boundaries."
  (concat "("
          "("
          (string-join
           (mapcar #'car
                   hl-todo-keyword-faces)
           "|")
          ")"
          (and (not (equal hl-todo-highlight-punctuation ""))
               (concat "[" hl-todo-highlight-punctuation "]"
                       (if hl-todo-require-punctuation "+" "*")))
          ")"))

(defun hl-todo-rg (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords.
This function interactively prompts for file types and directory to search.
REGEXP is the regular expression to search for.
FILES is the file type pattern to limit the search.
DIR is the base directory for the search."
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (my/hl-todo-generate-ripgrep-regexp)))
       (list regexp
             (rg-read-files)
             (read-directory-name "Base directory: " nil default-directory t)))))
  (rg regexp files dir))

(defun hl-todo-rg-project ()
  "Use `rg' to find all TODO or similar keywords in current project."
  (interactive)
  (unless (require 'rg nil t)
    (error "`rg' is not installed"))
  (rg-project (my/hl-todo-generate-ripgrep-regexp) "everything"))

(defun consult-todo--ripgrep (dir)
  "Function to use ripgrep to search keywords in DIR."
  (let* ((todo-buf (format "*consult-todo-dir %s*" dir))
         (grep-command "rg --color=auto --null -nH --no-heading -e ")
         (grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
         (grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
         (grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")
         (compilation-auto-jump-to-first-error nil)
         cache-p)
    (cl-letf ((compilation-buffer-name-function
               (lambda (&rest _) (format "%s" todo-buf))))
      (rgrep (my/hl-todo-generate-ripgrep-regexp) "* .*" dir)
      (let ((proc (get-buffer-process todo-buf)))
        (run-with-timer
         consult-todo-cache-threshold nil
         (lambda ()
           (when (and proc (process-live-p proc)
                      (eq (process-status proc) 'run))
             (message "consult-todo: dir %s is caching!" dir)
             (setq cache-p t))))
        (set-process-sentinel
         proc
         (lambda (_ event)
           (unwind-protect
               (when (string-equal "finished\n" event)
                 (let ((result
                        (consult-todo--format
                         (with-current-buffer todo-buf
                           (goto-char (point-min))
                           (cl-loop while (and (null (eobp))
                                               (condition-case nil
                                                   (progn
                                                     (compilation-next-error 1)
                                                     t)
                                                 (user-error nil)))
                                    when (save-excursion
                                           (save-match-data
                                             (text-property-search-forward 'font-lock-face 'match t)))
                                    collect
                                    (let* ((msg (get-text-property (point) 'compilation-message))
                                           (loc (compilation--message->loc msg))
                                           (line (compilation--loc->line loc))
                                           (col (compilation--loc->col loc))
                                           (file (caar (compilation--loc->file-struct loc)))
                                           (type (buffer-substring-no-properties
                                                  (prop-match-beginning it)
                                                  (prop-match-end it))))
                                      (list (file-name-nondirectory file)
                                            (number-to-string line)
                                            type
                                            (list (expand-file-name file compilation-directory)
                                                  line col)
                                            (car (or (rassoc type (consult-todo--narrow))
                                                     consult-todo-other))
                                            (string-trim
                                             (buffer-substring-no-properties
                                              (prop-match-end it)
                                              (line-end-position))))))))))
                   (if (null cache-p)
                       (condition-case nil
                           (consult-todo--dir result)
                         (quit (message "Quit")))
                     (setf (alist-get dir consult-todo--cache
                                      nil nil #'equal)
                           result)
                     (message "consult-todo: dir %s caching complete!" dir))))
             (kill-buffer todo-buf))))))))

(provide 'lib-hl-todo)
;;; lib-hl-todo.el ends here
