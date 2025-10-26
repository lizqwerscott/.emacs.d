;;; lib-dired.el --- dired utils function            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'dired-aux)

;;; Find file auto
(defcustom user/find-file-auto-regexp-list '("\\.xlsx?\\'" "\\.pptx?\\'" "\\.docx?\\'" "\\.mp4\\'" "\\.app\\'")
  "Open files via external program regexp list when using `find-file'."
  :group 'user
  :type '(list string))

(defun find-file-auto (orig-fun &rest args)
  "Open files via external program when using `find-file'.
ORIG-FUN is orign func, ARGS is orign fun arg."
  (let ((file (car args)))
    (if (cl-find-if (lambda (regexp)
                      (string-match regexp file))
                    user/find-file-auto-regexp-list)
        (shell-command-do-open
         (list (file-truename file)))
      (apply orig-fun args))))

;;; Functions
(when (version<= emacs-version "30.1")
  ;;; from emacs 30.1
  (defun shell-command-do-open (files)
    "Open each of FILES using an external program.
  This \"opens\" the file(s) using the external command that is most
  appropriate for the file(s) according to the system conventions."
    (let ((command shell-command-guess-open))
      (when (and (memq system-type '(windows-nt))
                 (equal command "start"))
        (setq command "open"))
      (if command
          (cond
           ((memq system-type '(ms-dos))
            (dolist (file files)
              (shell-command (concat command " " (shell-quote-argument file)))))
           ((memq system-type '(windows-nt))
            (dolist (file files)
              (w32-shell-execute command (convert-standard-filename file))))
           ((memq system-type '(cygwin))
            (dolist (file files)
              (call-process command nil nil nil file)))
           ((memq system-type '(darwin))
            (dolist (file files)
              (start-process (concat command " " file) nil command file)))
           (t
            (dolist (file files)
              (call-process command nil 0 nil file))))
        (error "Open not supported on this system")))))

(defun dired-copy-path ()
  "In `dired', copy file path to `kill-buffer'.
At 2nd time it copy current directory to `kill-buffer'."
  (interactive)
  (when-let* ((path (dired-file-name-at-point)))
    (if (string= path (current-kill 0 1))
        (setq path (dired-current-directory)))
    (message path)
    (kill-new path)))

(defun dired-do-open-default ()
  "Open the current default directory using the external app."
  (interactive)
  (shell-command-do-open
   (list (file-truename default-directory))))

(require 'casual-dired)

(defun dired-auto-sort-dir ()
  "Auto sort some dir."
  (interactive)
  (casual-dired--sort-by :date-added)
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (let ((file (dired-get-filename nil t)))
        (when (and file (not (file-directory-p file)))
          (dired-goto-file file)
          (throw 'found t)))
      (forward-line 1)))
  (when (not (eobp))
    (dired-move-to-filename)))

;;; from prot emacs config
(defvar prot-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

(defun prot-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (line-end-position))
    (if (re-search-forward subdir nil t (or arg nil))
        (progn
          (goto-char (match-beginning 1))
          (goto-char (line-beginning-position)))
      (goto-char pos))))

;;;###autoload
(defun prot-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (line-beginning-position))
    (if (re-search-backward subdir nil t (or arg nil))
        (goto-char (line-beginning-position))
      (goto-char pos))))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'prot-dired--limit-hist regexp))

(provide 'lib-dired)
;;; lib-dired.el ends here
