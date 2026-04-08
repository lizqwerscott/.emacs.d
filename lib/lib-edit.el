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

(defun +lizqwer/insert-repeated-numbers (format-str &optional outer-brackets separator)
  "Insert numbers based on format string.

FORMAT-STR: Format string for numbers:
  - Single number: 5 → generates 5 fixed values
  - Range: 1..5 → generates 1 to 5
  - Range with step: 1..10:2 → generates 1 to 10 with step 2
  - Fixed value with count: 5*42 → generates 5 times 42
  - Supports decimal numbers: 1.5..5.5, 0.1..1.0:0.2, 3*2.5

SEPARATOR: String separator between numbers. If nil, auto-detect based on language.
OUTER-BRACKETS: Type of outer brackets - 'square' for [], 'curly' for {}, 'paren' for (), or nil for none.
               Automatically detects language if nil."
  (interactive (let ((current-prefix-arg current-prefix-arg))
                 (list (read-string "Number format (e.g., 5, 1..5, 1..10:2, 5*42, 1.5..5.5, 0.1..1.0:0.2, 3*2.5):")
                       (when current-prefix-arg
                         (let ((choice (completing-read "Bracket type (square/curly/paren/none): "
                                                        '("square" "curly" "paren" "none") nil t)))
                           (cond ((equal choice "square") 'square)
                                 ((equal choice "curly") 'curly)
                                 ((equal choice "paren") 'paren)
                                 ((equal choice "none") nil)))))))
  (let* ((numbers (cond
                   ;; Fixed value with count: N*VALUE
                   ((string-match "^\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)$" format-str)
                    (let ((count (string-to-number (match-string 1 format-str)))
                          (value (string-to-number (match-string 2 format-str))))
                      (make-list count value)))
                   ;; Range with step: START..END:STEP
                   ((string-match "^\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\.\\.\\([0-9]+\\(?:\\.[0-9]+\\)?\\):\\\([0-9]+\\(?:\\.[0-9]+\\)?\\)$" format-str)
                    (let ((start (string-to-number (match-string 1 format-str)))
                          (end (string-to-number (match-string 2 format-str)))
                          (step (string-to-number (match-string 3 format-str))))
                      (number-sequence start end step)))
                   ;; Simple range: START..END
                   ((string-match "^\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\.\\.\\([0-9]+\\(?:\\.[0-9]+\\)?\\)$" format-str)
                    (let ((start (string-to-number (match-string 1 format-str)))
                          (end (string-to-number (match-string 2 format-str))))
                      (number-sequence start end)))
                   ;; Single number: generate that many fixed values
                   ((string-match "^[0-9]+\\(?:\\.[0-9]+\\)?$" format-str)
                    (let ((count (string-to-number format-str)))
                      (make-list count 1)))
                   (t (error "Invalid format string: %s" format-str))))
         (lang (when (null outer-brackets)
                 (cond
                  ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'clojure-mode 'scheme-mode) 'paren)
                  ((derived-mode-p 'c-mode 'c++-mode 'c++-ts-mode 'java-mode 'java-ts-mode 'js-ts-mode) 'curly)
                  ((derived-mode-p 'python-mode 'python-ts-mode) 'square)
                  ((derived-mode-p 'yaml-mode 'yaml-ts-mode) 'square)
                  (t 'square))))
         (bracket-type (or outer-brackets lang 'square))
         (auto-sep (cond
                    ((eq bracket-type 'paren) " ")
                    (t ", ")))
         (sep (or separator auto-sep))
         (num-strs (mapcar 'number-to-string numbers))
         (joined (mapconcat 'identity num-strs sep))
         (result (cond
                  ((eq bracket-type 'square) (format "[%s]" joined))
                  ((eq bracket-type 'curly) (format "{%s}" joined))
                  ((eq bracket-type 'paren) (format "(%s)" joined))
                  (t joined))))
    (insert result)))

(provide 'lib-edit)
;;; lib-edit.el ends here
