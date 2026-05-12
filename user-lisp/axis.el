;;; axis.el --- Axis                                 -*- lexical-binding: t; -*-

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

;; from https://emacs-china.org/t/emacs-axis/30404

;;; Code:

(defgroup axis '()
  "Statistics of how you play Emacs."
  :group 'applications)

(defcustom axis-db-location
  (concat user-emacs-directory "axis-data.sqlite")
  "A path to axis-db, which is a sqlite database."
  :group 'axis
  :type 'string)

(defvar axis-db nil
  "SQLite database connection for command-key tracking.")

(defun axis-db-init ()
  "Initialize the SQLite database with performance optimizations."
  (setq axis-db (sqlite-open axis-db-location))
  ;; Apply performance PRAGMAs
  (sqlite-execute axis-db "PRAGMA journal_mode = WAL;")
  (sqlite-execute axis-db "PRAGMA synchronous = NORMAL;")
  (sqlite-execute axis-db "PRAGMA cache_size = 2000;")
  ;; Create table if it doesn't exist
  (sqlite-execute
   axis-db
   "CREATE TABLE IF NOT EXISTS command_keys (
       command TEXT,
       key TEXT,
       count INTEGER,
       PRIMARY KEY (command, key)
     );"))

(defun axis-db-close ()
  "Close the SQLite database connection."
  (when axis-db
    (sqlite-close axis-db)
    (setq axis-db nil)))

(defun axis-record-command-and-keys ()
  "Record the current command and keys into the database."
  (when (and (commandp this-command)
             (symbolp this-command))
    (let ((key-str (key-description (this-command-keys))))
      (sqlite-execute
       axis-db
       "INSERT INTO command_keys (command, key, count) VALUES (?, ?, 1)
        ON CONFLICT(command, key) DO UPDATE SET count = count + 1;"
       (list (symbol-name this-command) key-str)))))

(defun axis-show-command-summary ()
  "Display commands and their associated keys with usage counts."
  (interactive)
  (let ((data
         (sqlite-select
          axis-db
          "SELECT command, SUM(count) FROM command_keys GROUP BY command
           ORDER BY SUM(count) DESC;"))
        )
    (with-output-to-temp-buffer "*Axis Commands & Keys*"
      (princ "Command Usage Statistics:\n\n")
      (dolist (entry data)
        (princ (format "Command: %s - %d times\n" (car entry) (cadr entry)))
        ))))

;;;###autoload
(define-minor-mode axis-mode
  "Record statistics of input commands and keys."
  :init-value nil

  (if axis-mode
      (progn
        (unless axis-db (axis-db-init))
        (add-hook 'pre-command-hook 'axis-record-command-and-keys)
        (message "Axis mode enabled."))
    (progn
      (remove-hook 'pre-command-hook 'axis-record-command-and-keys)
      (axis-db-close)
      (message "Axis mode disabled.")))
  )

(provide 'axis)
;;; axis.el ends here
