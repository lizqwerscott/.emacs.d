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

;;; edit tools
(defun ai-tools--generate-patch-diff (path orig-content new-content)
  "Generate a unified diff patch comparing ORIG-CONTENT and NEW-CONTENT for PATH.

PATH is the file path for which to generate the diff.  ORIG-CONTENT is
the original file content, which may be nil for new files.  NEW-CONTENT
is the modified file content, which may be nil for deleted files.

Return a unified diff string showing the changes between ORIG-CONTENT
and NEW-CONTENT, or nil if the file has not changed.  The diff uses
git-style headers and labels to properly handle file creations and
deletions."
  (let* (;; Get the path relative to the base directory.
         (rel-path (file-truename path))
         ;; Check if file has actually changed.
         (file-changed-p (not (equal orig-content new-content)))
         result)

    ;; Only generate diff if the file has actually changed.
    (when file-changed-p
      (let ((temp-orig (make-temp-file "gptel-diff-orig"))
            (temp-new (make-temp-file "gptel-diff-new")))

        ;; Write original content (or empty file for new files).
        (with-temp-buffer
          (when orig-content
            (insert orig-content))
          (write-region (point-min) (point-max) temp-orig nil 'silent))

        ;; Write new content or create empty file for deletions.
        (with-temp-buffer
          (when new-content
            (insert new-content))
          (write-region (point-min) (point-max) temp-new nil 'silent))

        ;; Generate diff and append to result.
        (with-temp-buffer
          ;; Add the standard git diff header, which allows diff-mode to create new files.
          (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

          ;; Use diff to generate a unified patch with the correct file path.
          (when (or orig-content new-content)
            (call-process "diff"
                          nil t nil "-u" "--label"
                          (if orig-content
                              (concat "a/" rel-path)
                            ;; Use /dev/null to denote file creations.
                            "/dev/null")
                          "--label"
                          (if new-content
                              (concat "b/" rel-path)
                            ;; Use /dev/null to denote file deletions.
                            "/dev/null")
                          temp-orig temp-new))

          ;; Append the diff to the result.
          (setq result (concat result (buffer-string))))

        ;; Clean up the temp files.
        (delete-file temp-orig)
        (delete-file temp-new)))
    result))


(defun ai-tools--edit-string (content old-string new-string &optional replace-all)
  "In CONTENT string, replace OLD-STRING with NEW-STRING.

If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, error if
multiple matches exist and replace only single occurrences.

Return the new content string if the replacement was successful, or signal
an error if it was not."
  (let ((case-fold-search nil))
    ;; Error if old-string and new-string are identical
    (when (string-equal old-string new-string)
      (error "No changes to make: old_string and new_string are exactly the same"))
    ;; Handle empty old-string specially.
    (if (string-empty-p old-string)
        (if (string-empty-p content)
            ;; If content is empty, return the new string.
            new-string
          ;; If content is not empty and old-string is empty, throw an error.
          (error "Cannot replace empty string in non-empty content"))
      ;; Normal case: old-string is not empty.
      (let* ((start 0)
             (matches 0)
             (match-positions '()))
        ;; Count matches and collect positions.
        (while (setq start (string-search old-string content start))
          (setq matches (1+ matches))
          (push start match-positions)
          (setq start (+ start (length old-string))))

        (cond
         ((= matches 0)
          (error "String to replace not found in file"))
         ((and (> matches 1) (not replace-all))
          (error
           (concat
            "Found %d matches of the string to replace, but replace_all is false. "
            "To replace all occurrences, set replace_all to true. To replace only one "
            "occurrence, please provide more context to uniquely identify the instance")
           matches))
         (t
          ;; Perform replacement(s)
          (if replace-all
              ;; Replace all occurrences (work backwards to preserve positions)
              (let ((result content))
                (dolist (pos (sort match-positions '>))
                  (setq result
                        (concat
                         (substring result 0 pos)
                         new-string
                         (substring result (+ pos (length old-string))))))
                result)
            ;; Replace single occurrence
            (let ((match-pos (car (reverse match-positions))))
              (concat
               (substring content 0 match-pos)
               new-string
               (substring content (+ match-pos (length old-string))))))))))))


(defun ai-tools--edit-file (path edits)
  "Edit file in PATH with new content.

PATH is the path to the file.

EDITS is a vector of edit operations, each containing :old_string and
:new_string. For compatibility with LLMs that don't support array arguments, a
JSON string representing an array is also accepted.

All edits are applied in sequence to the same file. Each edit requires exact
whitespace matching. If any edit fails, the entire operation fails.

Returns (content . new-content) on success. Signals an error if the file is not
found or if the edit operation fails."
  (unless (vectorp edits)
    (if (stringp edits)
        ;; Try to decode JSON string to vector.
        (condition-case nil
            (let ((decoded (json-parse-string edits :array-type 'vector :object-type 'plist)))
              (if (vectorp decoded)
                  (setq edits decoded)
                (error
                 "The 'edits' parameter must be an array, but the decoded JSON is not an array")))
          (error
           (error
            "The 'edits' parameter must be an array of objects, or a valid JSON string representing an array")))
      ;; Not a vector or string - invalid input.
      (error "The 'edits' parameter must be an array of objects, not %s" (type-of edits))))
  (let ((full-path (file-truename path)))
    (if (file-exists-p full-path)
        (when-let* ((content (with-temp-buffer (insert-file-contents full-path) (buffer-string)))
                    (new-content content))
          (cl-loop
           for edit across edits do
           (let ((old-text (plist-get edit :old_text))
                 (new-text (plist-get edit :new_text))
                 (replace-all (plist-get edit :replace_all)))

             (unless (and old-text new-text)
               (error
                "Each edit must contain old_text and new_text properties"))
             ;; Handle :json-false inputs for replace-all parameter.
             (setq replace-all
                   (and replace-all (not (eq replace-all :json-false))))
             (setq new-content
                   (ai-tools--edit-string new-content old-text new-text
                                          replace-all))))
          (cons content new-content))
      (error "The '%s' file not find" path))))

(defun ai-tools-edit-file-tool (path edits)
  "Edit file in PATH with new content.

PATH is the path to the file.

EDITS is a vector of edit operations, each containing :old_string and
:new_string. For compatibility with LLMs that don't support array
arguments, a JSON string representing an array is also accepted.

All edits are applied in sequence to the same file. Each edit requires
exact whitespace matching. If any edit fails, the entire operation
fails.

Returns nil on success. Signals an error if the file is not found or if the edit
operation fails."
  (condition-case err
      (pcase-let* ((`(,content . ,new-content) (ai-tools--edit-file path edits)))
        ;; generate diff buffer
        (let ((diff-buffer (get-buffer-create (format "*Diff for %s*" path)))
              (diff-text (ai-tools--generate-patch-diff path content new-content)))
          (if diff-text
              (progn
                (with-current-buffer diff-buffer
                  (insert diff-text)
                  (diff-mode))
                (pop-to-buffer-same-window diff-buffer)
                "Generate diff success.")
            "Generate diff buffer error, new content same with orign content")))
    (error
     (error-message-string err))))

(gptel-make-tool
 :name "edit_file"
 :function #'ai-tools-edit-file-tool
 :description
 (concat
  "Make multiple exact string replacements in a single file. "
  "Edits are applied sequentially in array order to the same file. "
  "Each edit requires exact whitespace matching. Do NOT include line numbers in old_text or new_text. "
  "If any edit fails, no changes are made. Returns null on success.")
 :confirm nil
 :include nil
 :args
 `((:name "path" :type string :description "Path to the file")
   (:name
    "edits"
    :type array
    :description "Array of edit operations to apply in sequence"
    :items
    (:type
     object
     :properties
     (:old_text
      (:type
       string
       :description
       ,(concat
         "Exact text to find and replace. Must match precisely including whitespace "
         "and newlines. Do NOT include line numbers."))
      :new_text (:type string :description "Text to replace the old_text with")
      :replace_all
      (:type
       boolean
       :description "If true, replace all occurrences. If false (default), error if multiple matches exist"))
     :required ["old_text" "new_text"])))
 :category "filesystem")

(provide 'ai-tools)
;;; ai-tools.el ends here
