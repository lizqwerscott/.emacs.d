;;; lib-git.el --- git                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: git

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

(require 'magit-section)
(require 'magit-status)
(require 'magit-extras)

(defun +magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names.

ARGS is `((REV AUTHOR DATE))'"
  (let* ((author (nth 1 (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))

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
                    (error
                     (cl-return
                      (progn
                        (goto-char (point-min))
                        (magit-status-goto-initial-section)))))))))

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

(require 'vc-git)
(require 'nerd-icons)
(require 'consult)

(defun consult-switch-git-status-buffer ()
  "Parse git status from an expanded path and switch to a file.
The completion candidates include the Git status of each file."
  (interactive)
  (let ((repo-root (vc-git-root default-directory)))
	(if (not repo-root)
		(message "Not inside a Git repository.")
	  (let* ((expanded-root (expand-file-name repo-root))
			 (command-to-run (format "git -C %s status --porcelain=v1"
									 (shell-quote-argument expanded-root)))
			 (cmd-output (shell-command-to-string command-to-run))
			 (target-files
			  (let ((staged-files)
                    (unstaged-files)
                    (untracked-files)
                    (new-files))
				(dolist (line (split-string cmd-output "\n" t) ;; (nreverse files)
                              `(("staged" . ,staged-files)
                                ("unstaged" . ,unstaged-files)
                                ("untracked" . ,untracked-files)))
				  (when (> (length line) 3)
					(let ((status (substring line 0 2))
						  (path-info (substring line 3)))
					  ;; Handle rename specially
					  (if (string-match "^R" status)
						  (let* ((paths (split-string path-info " -> " t))
								 (new-path (cadr paths)))
							(when new-path
							  (push (cons (format "R %s" new-path) new-path) new-files)))
						;; Modified or untracked
                        (cond
                         ((string-prefix-p "M" status)
                          (push path-info staged-files))
                         ((string-prefix-p " M" status)
                          (push path-info unstaged-files))
                         ((string-match "\?\?" status)
                          (push path-info untracked-files))))))))))
		(if (not target-files)
			(message "No modified or renamed files found.")
		  (let* ((candidates (cl-loop for (gname . items) in target-files
                                      append (mapcar (lambda (it) (cons it gname)) items)))
		    	 (selection (consult--read (mapcar #'car candidates)
                                           :prompt "Switch to buffer (Git modified): "
                                           :predicate nil
                                           :require-match t
                                           :group (lambda (cand transform)
                                                    (if transform
                                                        (concat (nerd-icons-icon-for-file cand)
                                                                " "
                                                                cand)
                                                      (alist-get cand candidates nil nil #'equal)))
                                           :sort nil)))
		    (when selection
		      (let ((file-path selection))
		    	(when file-path
		    	  (find-file (expand-file-name file-path expanded-root)))))))))))

(provide 'lib-git)
;;; lib-git.el ends here
