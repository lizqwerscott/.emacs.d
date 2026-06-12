;;; lib-denote-journal.el ---  denote journal                      -*- lexical-binding: t; -*-

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

(require 'denote-journal)

(defun denote-week-report-new-or-existing-entry ()
  "Denote week report."
  (interactive)
  (let ((denote-journal-keyword (list "journal" "report"))
        (denote-templates '((journal . denote-week-report-template)))
        (denote-journal-interval 'weekly))
    (denote-journal-new-or-existing-entry)))

(defun denote-journal-path-to-new-or-existing-entry-filter-report (&optional date interval)
  "Return path to existing or new journal file.
With optional DATE, do it for that date, else do it for today.  DATE is
a string and has the same format as that covered in the documentation of
the `denote' function.  It is internally processed by `denote-valid-date-p'.

If there are multiple journal entries for the date, prompt for one among
them using minibuffer completion.  If there is only one, return it.  If
there is no journal entry, create it.

With optional INTERVAL as a symbol among those accepted by
`denote-journal-interval', match DATE to INTERVAL and then return the
results accordingly.  If INTERVAL is nil, then it has the same measing
as `daily', per `denote-journal-interval'."
  (let* ((internal-date (denote-journal--date-in-interval-p (or date (current-time)) interval))
         (files (cl-remove-if (lambda (file)
                                (cl-find "report"
                                         (denote-retrieve-front-matter-keywords-value file
                                                                                      (denote-filetype-heuristics file))
                                         :test #'equal))
                              (denote-journal--get-entry internal-date interval)))
         (denote-kill-buffers nil))
    (if files
        (denote-journal-select-file-prompt files)
      (save-window-excursion
        (denote-journal-new-entry date)
        (save-buffer)
        (buffer-file-name)))))

(defun denote-journal--all-entries ()
  "Return all journal entries sorted by date (ascending)."
  (let ((denote-directory (denote-journal-directory))
        (files (denote-directory-files nil nil nil nil t)))
    (sort (cl-remove-if-not
           (lambda (file)
             (and (denote-journal-file-is-journal-p file)
                  (not (member (file-name-extension file) '("html" "pdf" "tex")))
                  t))
           files)
          (lambda (a b)
            (string< (denote-retrieve-filename-identifier a)
                     (denote-retrieve-filename-identifier b))))))

(defun denote-journal--current-buffer-date ()
  "Return the identifier date of the current buffer if it's a journal entry.
Return nil otherwise."
  (when-let* ((file (buffer-file-name))
              (id (denote-retrieve-filename-identifier file)))
    id))

;;;###autoload
(defun denote-journal-goto-previous-entry ()
  "Go to the previous journal entry before the current one.
If the current buffer is not a journal entry, jump to the most
recent journal entry (the last one chronologically)."
  (interactive)
  (let* ((all (denote-journal--all-entries))
         (current-id (denote-journal--current-buffer-date))
         (target
          (if current-id
              ;; Find the entry before current
              (catch 'found
                (let ((prev nil))
                  (dolist (f all)
                    (let ((fid (denote-retrieve-filename-identifier f)))
                      (when (string= fid current-id)
                        (throw 'found prev))
                      (setq prev f)))
                  nil))
            ;; Not on a journal entry → jump to latest
            (car (last all)))))
    (if (not target)
        (user-error "No previous journal entry found")
      (find-file target))))

;;;###autoload
(defun denote-journal-goto-next-entry ()
  "Go to the next journal entry after the current one.
If the current buffer is not a journal entry, signal an error."
  (interactive)
  (let* ((all (denote-journal--all-entries))
         (current-id (denote-journal--current-buffer-date))
         (target
          (if current-id
              (catch 'found
                (let ((found-current nil))
                  (dolist (f all)
                    (when found-current
                      (throw 'found f))
                    (when (string= (denote-retrieve-filename-identifier f) current-id)
                      (setq found-current t)))
                  nil))
            (user-error "Current buffer is not a journal entry"))))
    (if (not target)
        (user-error "No next journal entry found")
      (find-file target))))

(provide 'lib-denote-journal)
;;; lib-denote-journal.el ends here
