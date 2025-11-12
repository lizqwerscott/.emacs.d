;;; macher-utils.el --- some macher utils function   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

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

(require 'macher)

(declare-function imenu--flatten-index-alist "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun get-file-outline (path)
  "Return a string representation of the outline for PATH.

The outline is generated from the imenu index and includes the name and
position of each item."
  (when-let* ((path (file-truename path))
              (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (string-join
       (cl-loop for (current . rest) on (cdr (imenu--flatten-index-alist (imenu--make-index-alist) t))
                for next = (car rest)
                collect (concat (substring-no-properties (car current))
                                " [ start pos: "
                                (number-to-string (marker-position (cdr current)))
                                " end pos: "
                                (number-to-string
                                 (if next
                                     (marker-position (cdr next))
                                   (point-max)) )
                                " ]"))
       "\n"))))

(defun my/get-surrounding-chars-with-cursor (&optional n-chars)
  "Get content in cursor.
N-CHARS is max size."
  (let* ((n (or n-chars 500))
         (pt (point))
         (buf-min (point-min))
         (buf-max (point-max))
         (start-pos (max buf-min (- pt n)))
         (end-pos   (min buf-max (+ pt n)))
         before after)
    (setq before (buffer-substring-no-properties start-pos pt))
    (setq after  (buffer-substring-no-properties pt end-pos))
    (concat before "{CURSOR}" after)))

(defun macher--implement-with-cursor-prompt (input is-selected)
  "Generate an implementation prompt for INPUT in the current buffer.

IS-SELECTED specifies whether the input comes from the selected region."
  (let* ((workspace (macher-workspace))
         (filename (buffer-file-name))
         (relpath
          (when filename
            (file-relative-name filename (macher--workspace-root workspace))))
         (source-description
          (cond
           ;; No file associated with buffer.
           ((null filename)
            "")
           ;; Directory case.
           ((file-directory-p filename)
            (format "The request was sent from the workspace directory `%s`. " relpath))
           ;; Regular file case.
           (t
            (let* ((lang
                    ;; Use gptel's internal mode formatting method, with a fallback on error in case
                    ;; the method gets removed or the signature changes (errors should also be
                    ;; picked up by tests, so we'll notice such a change eventually).
                    (condition-case nil
                        (gptel--strip-mode-suffix major-mode)
                      (error
                       (format-mode-line mode-name)))))
              (format (concat
                       "The request was sent from the %s file `%s` in the workspace. "
                       "If the request text appears as a comment or placeholder in the file, "
                       "replace it with the actual implementation. ")
                      lang relpath))))))
    (format (concat
             "TASK: Implement the following request using workspace tools.\n\n"
             "INSTRUCTIONS:\n"
             "1. Read and understand the implementation request below\n"
             "2. Use the workspace tools to edit files as needed\n"
             "3. Create working, complete code that fulfills the request\n\n"
             "%s"
             "\n\nIMPLEMENTATION REQUEST:\n\n%s%s")
            source-description
            input
            (unless is-selected
              (format "\n\nCursor place content:\n\n%s"
                      (my/get-surrounding-chars-with-cursor 500))))))

(require 'ai-tools)

(defun macher--read-tools-with-ai-tools (context make-tool-function)
  "Generate read-only tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of read-only tools that allow the LLM to inspect files
in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (append (macher--read-tools context make-tool-function)
          (list
           (gptel-get-tool
            (list "filesystem"
                  "list_directory"))
           (gptel-get-tool
            (list "filesystem"
                  "find_files")))))

(provide 'macher-utils)
;;; macher-utils.el ends here
