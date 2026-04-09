;;; lib-edit.el --- edit functions                   -*- lexical-binding: t; -*-

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

(defun my/copy-no-properties ()
  "Copy the mark place with no properties."
  (interactive)
  (when (use-region-p)
    (kill-new
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  "Insert numbered lines in rectangle selection.
When called interactively, prompt for START-AT, END-AT, STEP, and FORMAT.
START-AT is the number to start counting from.
END-AT is the number to end counting at.
STEP is the increment between numbers.
FORMAT is the format string for each number (e.g., \"%d \" or \"%02d.\")."
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer.
Move point to the position that is PERCENT percent through the buffer.
For example, (goto-percent 50) moves to the middle of the buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  "Scroll up one third of the window height."
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  "Scroll down one third of the window height."
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun scroll-other-window-up-1/3 ()
  "Scroll other window up one third of the window height."
  (interactive)
  (scroll-other-window (/ (window-body-height) 3)))

(defun scroll-other-window-down-1/3 ()
  "Scroll other window down one third of the window height."
  (interactive)
  (scroll-other-window-down (/ (window-body-height) 3)))

;;;###autoload
(defun toggle-sub-word-or-super-word ()
  "Toggle between subword mode and superword mode."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun get-file-path ()
  "Get file path."
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun +lizqwer/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (car (last (file-name-split (get-file-path))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun +lizqwer/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (get-file-path)))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))


(defmacro +lizqwer-test-should-equal (format-string expected)
  "Test that FORMAT-STRING produces EXPECTED list of numbers."
  `(should (equal (+lizqwer/parse-number-format ,format-string) ,expected)))

(defmacro +lizqwer-test-should-error (format-string)
  "Test that FORMAT-STRING throws an error."
  `(should-error (+lizqwer/parse-number-format ,format-string)
                 :type 'error))

(defun +lizqwer/parse-number-format (format-str)
  "Parse number format string and return list of numbers.

FORMAT-STR: Format string for numbers:

  - Single number: 5 → generates 5 fixed values (0,0,0,0,0)

  - Range: 1..5 → generates 1 to 5 (1,2,3,4,5)

  - Range with step: 1..10:2 → generates 1 to 10 with step 2 (1,3,5,7,9)

  - Fixed value with count: 5*42 → generates 5 times 42 (42,42,42,42,42)

  - Supports decimal numbers: 1.5..5.5, 0.1..1.0:0.2, 3*2.5

  - Supports negative numbers: -5..5, -3..-1:0.5

  - Whitespace is ignored: \"  1 .. 5 * 2 \" works fine

Returns list of numbers.

Examples:
  (+lizqwer/parse-number-format \"3\")        ; => (0 0 0)
  (+lizqwer/parse-number-format \"1..5\")     ; => (1 2 3 4 5)
  (+lizqwer/parse-number-format \"1..10:2\")  ; => (1 3 5 7 9)
  (+lizqwer/parse-number-format \"5*42\")     ; => (42 42 42 42 42)
  (+lizqwer/parse-number-format \"-2..2\")    ; => (-2 -1 0 1 2)
  (+lizqwer/parse-number-format \"3*2.5\")    ; => (2.5 2.5 2.5)"
  (let* ((str (string-trim format-str))  ; Trim whitespace first
         (parsed-number "\\(?:-?[0-9]+\\(?:\\.[0-9]+\\)?\\)")
         (count-number "[0-9]+"))
    (cond
     ;; Fixed value with count: N*VALUE or N * VALUE
     ((string-match (concat "^\\s-*\\(" count-number "\\)\\s-*\\*\\s-*\\(" parsed-number "\\)\\s-*$") str)
      (let* ((count (string-to-number (match-string 1 str)))
             (value (string-to-number (match-string 2 str))))
        (when (<= count 0)
          (error "Count must be positive: %s" count))
        (make-list count value)))

     ;; Range with step: START..END:STEP
     ((string-match (concat "^\\s-*\\(" parsed-number "\\)\\s-*\\.\\.\\s-*\\(" parsed-number "\\)\\s-*:\\s-*\\(" parsed-number "\\)\\s-*$") str)
      (let ((start (string-to-number (match-string 1 str)))
            (end (string-to-number (match-string 2 str)))
            (step (string-to-number (match-string 3 str))))
        (when (zerop step)
          (error "Step cannot be zero: %s" format-str))
        (when (and (plusp step) (> start end))
          (error "Invalid range: start (%f) > end (%f) with positive step" start end))
        (when (and (minusp step) (< start end))
          (error "Invalid range: start (%f) < end (%f) with negative step" start end))
        (number-sequence start end step)))

     ;; Simple range: START..END
     ((string-match (concat "^\\s-*\\(" parsed-number "\\)\\s-*\\.\\.\\s-*\\(" parsed-number "\\)\\s-*$") str)
      (let ((start (string-to-number (match-string 1 str)))
            (end (string-to-number (match-string 2 str))))
        (number-sequence start end)))

     ;; Single number: generate that many fixed values (count zeros)
     ((string-match (concat "^\\s-*\\(" parsed-number "\\)\\s-*$") str)
      (let ((count (string-to-number (match-string 1 str))))
        (unless (and (numberp count) (>= count 0))
          (error "Count must be non-negative: %s" count))
        (make-list (truncate count) 0)))

     (t
      (error "Invalid format string: %s\nExpected formats:\n  N        → N zeros\n  N*M      → M copies of N\n  N..M     → N to M\n  N..M:P   → N to M with step P"
             format-str)))))

(defun +lizqwer/detect-bracket-type (outer-brackets)
  "Detect bracket type based on current mode or use provided OUTER-BRACKETS.

OUTER-BRACKETS: Type of outer brackets - `square' for [], `curly' for {},
`paren' for (), or nil for auto-detect.

BRACKET-TYPE: Symbol: `'square', `'curly', `'paren', or nil."
  (or outer-brackets
      (cond
       ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'clojure-mode 'scheme-mode) 'paren)
       ((derived-mode-p 'c-mode 'c++-mode 'c++-ts-mode 'java-mode 'java-ts-mode 'js-ts-mode) 'curly)
       ((derived-mode-p 'python-mode 'python-ts-mode) 'square)
       ((derived-mode-p 'yaml-mode 'yaml-ts-mode) 'square)
       (t 'square))))

(defun +lizqwer/detect-separator (bracket-type separator)
  "Detect separator based on bracket type or use provided SEPARATOR.

BRACKET-TYPE: Symbol: `'square', `'curly', `'paren', or nil.
SEPARATOR: String separator between numbers, or nil for auto-detect.

Returns string separator."
  (or separator
      (cond
       ((eq bracket-type 'paren) " ")
       (t ", "))))

(defun +lizqwer/format-number-list (numbers bracket-type separator)
  "Format list of numbers with brackets and separator.

NUMBERS: List of numbers.
BRACKET-TYPE: Symbol: `'square', `'curly', `'paren', or nil.
SEPARATOR: String separator between numbers.

Returns formatted string."
  (let* ((num-strs (mapcar 'number-to-string numbers))
         (joined (mapconcat 'identity num-strs separator))
         (result (cond
                  ((eq bracket-type 'square) (format "[%s]" joined))
                  ((eq bracket-type 'curly) (format "{%s}" joined))
                  ((eq bracket-type 'paren) (format "(%s)" joined))
                  (t joined))))
    result))

(defun +lizqwer/insert-repeated-numbers (format-str &optional outer-brackets separator)
  "Insert numbers based on format string.

FORMAT-STR: Format string for numbers:
  - Single number: 5 → generates 5 fixed values
  - Range: 1..5 → generates 1 to 5
  - Range with step: 1..10:2 → generates 1 to 10 with step 2
  - Fixed value with count: 5*42 → generates 5 times 42
  - Supports decimal numbers: 1.5..5.5, 0.1..1.0:0.2, 3*2.5

SEPARATOR: String separator between numbers. If nil, auto-detect based on
language.
OUTER-BRACKETS: Type of outer brackets - `square' for [], `curly' for {},
`paren' for (), or nil for none. Automatically detects language if nil."
  (interactive (let ((current-prefix-arg current-prefix-arg))
                 (list (read-string "Number format (e.g., 5, 1..5, 1..10:2, 5*42, 1.5..5.5, 0.1..1.0:0.2, 3*2.5):")
                       (when current-prefix-arg
                         (let ((choice (completing-read "Bracket type (square/curly/paren/none): "
                                                        '("square" "curly" "paren" "none") nil t)))
                           (cond ((equal choice "square") 'square)
                                 ((equal choice "curly") 'curly)
                                 ((equal choice "paren") 'paren)
                                 ((equal choice "none") nil)))))))
  (let* ((numbers (+lizqwer/parse-number-format format-str))
         (bracket-type (+lizqwer/detect-bracket-type outer-brackets))
         (sep (+lizqwer/detect-separator bracket-type separator))
         (result (+lizqwer/format-number-list numbers bracket-type sep)))
    (insert result)))

(provide 'lib-edit)
;;; lib-edit.el ends here
