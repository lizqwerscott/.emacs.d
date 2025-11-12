;;; ai-tools.el --- ai tools                         -*- lexical-binding: t; -*-

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

;;; ai-glob-tool.el --- Simple AI file search tool using file-expand-wildcards -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(require 'gptel)

(defun ai-tools--read-ignore-list (dir filename)
  "Read ignore patterns from FILENAME inside DIR.
return ignore patterns as a list of strings.
Empty lines and comments (#...) are ignored."
  (let ((ignore-file (expand-file-name filename dir)))
    (when (file-exists-p ignore-file)
      (split-string
       (with-temp-buffer
         (insert-file-contents ignore-file)
         (replace-regexp-in-string "^#.*" "" (buffer-string)))
       "\n" t "[[:space:]]+"))))

;;; glob tool
(defun ai-tools--glob-sort-files (files)
  "Sort FILES by modification time (newest first)."
  (sort files
        (lambda (a b)
          (> (float-time (file-attribute-modification-time (file-attributes a)))
             (float-time (file-attribute-modification-time (file-attributes b)))))))


(defun ai-tools--apply-ignore-rules (files dir respect-git respect-ai)
  "Filter FILES in DIR.
Removing paths that match .gitignore or .aiignore patterns in DIR. If
RESPECT-GIT or RESPECT-AI are nil, the corresponding ignore file is not
considered."
  (let* ((git-ignore (and respect-git (ai-tools--read-ignore-list dir ".gitignore")))
         (ai-ignore  (and respect-ai (ai-tools--read-ignore-list dir ".aiignore")))
         (ignores (append git-ignore ai-ignore)))
    (seq-remove
     (lambda (f)
       (seq-some (lambda (pat)
                   (string-match-p (wildcard-to-regexp pat)
                                   (file-relative-name f dir)))
                 ignores))
     files)))

(cl-defun ai-tools-glob-tool (pattern dir-path &optional (case-sensitive nil) (respect-git-ignore t) (respect-ai-ignore t))
  "Find files matching PATTERN using `file-expand-wildcards'.

Arguments:
  PATTERN (string):The glob pattern to match, e.g. \"src/**/*.el\" or \"*.org\".
  DIR-PATH (string, optional): Directory in which to search.
  CASE-SENSITIVE (boolean, optional): If non-nil, matching is case-sensitive.
  RESPECT-GIT-IGNORE (boolean, optional): If non-nil, obey .gitignore rules.
  RESPECT-AI-IGNORE (boolean, optional): If non-nil, obey .aiignore rules.

Returns a list of absolute file paths, sorted by modification time (newest
first). If no files are found, returns nil and displays a message."
  (let* ((dir dir-path)
         (default-directory (file-name-as-directory dir))
         ;; Adjust pattern case if case-insensitive search
         (pattern (if (and (not case-sensitive)
                           (string-match-p "[A-Z]" pattern))
                      (downcase pattern)
                    pattern))
         (files (file-expand-wildcards pattern t))
         (filtered (ai-tools--apply-ignore-rules
                    files dir
                    (or respect-git-ignore t)
                    (or respect-ai-ignore t)))
         (sorted (ai-tools--glob-sort-files filtered)))
    (if (null sorted)
        (format "No files found matching: %s in %s" dir pattern)
      (format "Found %d files matching '%s' (sorted by modification time):\n%s"
              (length sorted)
              pattern
              (string-join sorted "\n")))))

;; Register the tool with GPTel
(gptel-make-tool
 :name "find_files"
 :function #'ai-tools-glob-tool
 :description
 "Find files matching a given glob pattern using Emacs' file-expand-wildcards.
Returns a list of files sorted by modification time (newest first). Supports ignore rules and recursive patterns."
 :args (list
        '(:name "pattern"
                :type string
                :description "The glob pattern to match, e.g. '**/*.el' or '*.org'")
        '(:name "dir-path"
                :type string
                :description "Optional directory path to search within")
        '(:name "case-sensitive"
                :type boolean
                :description "Whether the search is case-sensitive (default: false)"
                :optional t)
        '(:name "respect-git-ignore"
                :type boolean
                :description "Whether to obey .gitignore (default: true)"
                :optional t)
        '(:name "respect-ai-ignore"
                :type boolean
                :description "Whether to obey .aiignore (default: true)"
                :optional t))
 :category "filesystem")

;;; ls tools
(defun ai-tools--should-ignore (filename patterns)
  "Return non-nil if FILENAME matches any glob PATTERNS.
Each pattern may include '*' and '?' wildcards."
  (when (and patterns (listp patterns))
    (seq-some
     (lambda (pattern)
       (let* ((regex-pattern
               (replace-regexp-in-string
                "\\?" "."
                (replace-regexp-in-string
                 "\\*" ".*"
                 (replace-regexp-in-string
                  "[.+^${}()|[\\]\\]" "\\\\\\&"
                  pattern))))
              (regex (concat "^" regex-pattern "$")))
         (string-match-p regex filename)))
     patterns)))

(defun ai-tools--ls-collect-files (dir ignore respect-git respect-ai)
  "Collect and filter files in DIR based on IGNORE patterns and ignore files.
Respects .gitignore and .aiignore when requested.

RESPECT-GIT (boolean, optional): If non-nil, obey .gitignore rules.
RESPECT-AI (boolean, optional): If non-nil, obey .aiignore rules."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (entries (directory-files dir t "^[^.]" t))
         (git-ignore (and respect-git (ai-tools--read-ignore-list dir ".gitignore")))
         (ai-ignore  (and respect-ai (ai-tools--read-ignore-list dir ".aiignore")))
         (ignore-list (append ignore git-ignore ai-ignore))
         results)
    (dolist (f entries)
      (unless (ai-tools--should-ignore (file-name-nondirectory f) ignore-list)
        (when (file-exists-p f)
          (let* ((attrs (file-attributes f))
                 (is-dir (car attrs))
                 (size (if is-dir 0 (nth 7 attrs)))
                 (mtime (nth 5 attrs)))
            (push (list
                   (cons 'name (file-name-nondirectory f))
                   (cons 'path f)
                   (cons 'isDirectory is-dir)
                   (cons 'size size)
                   (cons 'modifiedTime mtime))
                  results)))))
    ;; sort: directories first, then alphabetically
    (setq results
          (sort results
                (lambda (a b)
                  (let ((ad (alist-get 'isDirectory a))
                        (bd (alist-get 'isDirectory b)))
                    (if (eq ad bd)
                        (string< (alist-get 'name a) (alist-get 'name b))
                      ad)))))
    results))

(defun ai-tools-ls-tool (dir-path &optional ignore respect-git-ignore respect-ai-ignore)
  "List directory contents for DIR-PATH, respecting ignore patterns.

Arguments:
  DIR-PATH (string): The directory path to list.
  IGNORE (list of string): List of glob patterns to ignore.
  RESPECT-GIT-IGNORE (boolean): Whether to respect .gitignore (default: t).
  RESPECT-AI-IGNORE (boolean): Whether to respect .aiignore (default: t).

Returns a list of alists describing files, each with keys:
  - name: filename
  - path: absolute path
  - isDirectory: non-nil if it is a directory
  - size: file size in bytes
  - modifiedTime: last modification time."
  (condition-case err
      (let* ((dir (or dir-path default-directory)))
        (unless (file-directory-p dir)
          (error "Path is not a directory: %s" dir))
        (let ((entries (ai-tools--ls-collect-files
                        dir ignore
                        (or respect-git-ignore t)
                        (or respect-ai-ignore t))))
          (if (null entries)
              (format "Directory %s is empty." dir)
            (format "Listed %d entries in %s (sorted by modification time):\n%s"
                    (length entries)
                    dir
                    (string-join (mapcar (lambda (item)
                                           (alist-get 'name item))
                                         entries)
                                 "\n")))))
    (error
     (format "Error listing directory: %s" (error-message-string err)))))

;; Register with GPTel
(gptel-make-tool
 :name "list_directory"
 :function #'ai-tools-ls-tool
 :description
 "List the names of files and subdirectories directly within a specified directory path.
Supports glob-style ignore patterns and optionally respects .gitignore and .aiignore files."
 :args (list
        '(:name "dir-path"
                :type string
                :description "The path to the directory to list")
        '(:name "ignore"
                :type array
                :description "List of glob patterns to ignore"
                :optional t)
        '(:name "respect-git-ignore"
                :type boolean
                :description "Whether to respect .gitignore, Only available in git repositories. (default: true)"
                :optional t)
        '(:name "respect-ai-ignore"
                :type boolean
                :description "Whether to respect .aiignore (default: true)"
                :optional t))
 :category "filesystem")

(provide 'ai-tools)
;;; ai-tools.el ends here
