;;; project-list.el --- project bmenu               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

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

(require 'project)

(defface project-list-marked-face '((t :inherit warning))
  "Face used for displaying marked buffers."
  :group 'project-list)

(defface project-list-project-root-face '((t (:inherit completions-annotations)))
  "Face used for the project-root."
  :group 'project-list)

(defvar project-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'project-list-visit-project)
    (define-key map (kbd "g") 'project-list-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "o") 'project-list-open-project)
    (define-key map (kbd "k") 'project-list-forget-project)
    (define-key map (kbd "A") 'project-list-add-project)
    (define-key map (kbd "n") 'project-list-forward-line)
    (define-key map (kbd "p") 'project-list-backward-line)
    map)
  "Keymap for `project-list-mode'.")

(define-derived-mode project-list-mode special-mode "Project List"
  "Major mode for listing known projects.
\\{project-list-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;; This makes things less ugly for users with a non-nil
  ;; `show-trailing-whitespace'.
  (setq show-trailing-whitespace nil)
  ;; disable `show-paren-mode' buffer-locally
  (if (bound-and-true-p show-paren-mode)
      (setq-local show-paren-mode nil)))

(defun project-list-refresh ()
  "Refresh the project list."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (project-list-insert-projects)))

(defun project-list-insert-projects ()
  "Insert all known projects into the buffer."
  (let ((projects (project-known-project-roots))
        (first-line-point nil))
    (if (null projects)
        (insert "No known projects.\n")
      (insert (propertize (format "%-8s %-30s %s\n" "Type" "Name" "Path")
                          'face 'bold
                          'project-list-title t))
      (insert (propertize (make-string 80 ?-)
                          'face 'shadow
                          'project-list-title t))
      (insert "\n")
      (setq first-line-point (point))
      (dolist (project-root projects)
        (let* ((pr (project--find-in-directory project-root))
               (type (cond
                      ((null pr) "Unknown")
                      ((eq (car pr) 'transient) "Transient")
                      (t (symbol-name (car pr)))))
               (name (file-name-nondirectory (directory-file-name project-root)))
               (type-face (cond
                           ((null pr) 'error)
                           ((eq (car pr) 'transient) 'warning)
                           ((eq (car pr) 'vc) 'success)
                           (t 'font-lock-type-face)))
               (name-face (if (file-directory-p project-root)
                              'font-lock-function-name-face
                            'shadow)))
          (insert (propertize (format "%-8s " type) 'face type-face))
          (insert (propertize (format "%-30s " name) 'face name-face))
          (insert (propertize project-root
                              'face 'project-list-project-root-face
                              'project-root-path project-root)
                  "\n"))))
    (goto-char first-line-point)))

(defun project-list-open-project ()
  "Use `dired' open project root dir."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (dired-other-window project)))

(defun project-list-visit-project ()
  "Visit the project at point."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (project-switch-project project)))

(defun project-list-get-project-at-point ()
  "Get the project root at point."
  (save-excursion
    (when-let* ((end (line-end-position))
                (pos (next-single-property-change (point) 'project-root-path nil end)))
      (get-text-property pos 'project-root-path))))

(defun project-list-forget-project ()
  "Forget the project at point."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (project-forget-project project)
    (project-list-refresh)))

(defun project-list-skip-properties (props direction)
  (while (and (not (eobp))
	          (let ((hit nil))
		        (dolist (prop props hit)
		          (when (get-text-property (point) prop)
		            (setq hit t)))))
    (forward-line direction)
    (beginning-of-line)))

(defun project-list-backward-line (&optional arg)
  "Move backwards ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (while (> arg 0)
    (forward-line -1)
    (when (get-text-property (point) 'project-list-title)
      (goto-char (point-max))
      (beginning-of-line))
    ;; Handle the special case of no project list.
    (when (get-text-property (point) 'project-list-title)
      (forward-line 1)
      (setq arg 1))
    (decf arg)))

(defun project-list-forward-line (&optional arg)
  "Move forward ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (when (eobp)
    (goto-char (point-min)))
  (when (get-text-property (point) 'project-list-title)
    (when (> arg 0)
      (decf arg))
    (project-list-skip-properties '(project-list-title) 1))
  (if (< arg 0)
      (project-list-backward-line (- arg))
    (while (> arg 0)
      (forward-line 1)
      (when (eobp)
	    (goto-char (point-min)))
      (decf arg)
      (project-list-skip-properties '(project-list-title) 1))))

;;;###autoload
(defun project-list-projects ()
  "Display a list of known projects."
  (interactive)
  (let ((buffer (get-buffer-create "*Project List*")))
    (with-current-buffer buffer
      (project-list-mode)
      (project-list-refresh))
    (switch-to-buffer buffer)))

(provide 'project-list)
;;; project-list.el ends here
