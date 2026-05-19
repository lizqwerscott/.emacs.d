;;; gptel-translate.el --- Translate with gptel      -*- lexical-binding: t; -*-

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

;; Provide functions to translate buffer/region content using gptel,
;; with paragraph-by-paragraph processing and original/translation
;; side-by-side display.

;;; Code:

(require 'gptel)

;;; Customization

(defgroup gptel-translate nil
  "Options for buffer translation with gptel."
  :group 'gptel
  :prefix "gptel-translate-")

(defcustom gptel-translate-backend nil
  "Backend name for gptel translate requests.
If nil, uses `gptel-backend'."
  :type '(choice (string :tag "Backend name")
                 (const :tag "Use default" nil))
  :group 'gptel-translate)

(defcustom gptel-translate-model nil
  "Model symbol for gptel translate requests.
If nil, uses `gptel-model'."
  :type '(choice (symbol :tag "Model symbol")
                 (const :tag "Use default" nil))
  :group 'gptel-translate)

(defcustom gptel-translate-target-language "Chinese"
  "Target language for translation.
This is used in the prompt sent to the LLM."
  :type 'string
  :group 'gptel-translate)

(defcustom gptel-translate-system-prompt "You are a professional translator."
  "System prompt used by ChatGPT."
  :group 'gptel-translate
  :type 'string)

(defcustom gptel-translate-user-prompt (concat "You will be provided with text delimited by triple backticks, your task is to translate the wrapped text into {{to}}."
                                               "## Quality & Style Mandates: \n"
                                               "1. **Tone & Style Matching:** You MUST analyze and match the tone of the original text (e.g., formal, informal, technical, humorous, poetic).\n"
                                               "2. **Cultural Adaptation:** Translate idioms, metaphors, and cultural references into their closest cultural equivalents in {{to}}."
                                               "## Formatting & Technical Rules\n"
                                               "1. **Non-Translatable Entities:** The following MUST remain in their original form:\n"
                                               "* Proper Nouns & Brands (e.g., \"NVIDIA\", \"ChatGPT\"), unless an official, widely-used translation exists in {{to}}.\n"
                                               "* Code blocks, inline code, URLs, file paths, and technical acronyms (e.g., API, CSS).\n"
                                               "2. **Spacing Rule for Retained Original Text (applies when {{to}} is Chinese):**\n"
                                               "For any entity that remains untranslated according to Rule 1, a single half-width space (U+0020) MUST be inserted between that entity and any adjacent Chinese character.\n"
                                               "* A space is required both before and after the entity if both sides are Chinese characters.\n"
                                               "* If the entity appears at the beginning of a sentence, no leading space is needed before it; if at the end, no trailing space is needed before the sentence‑ending punctuation.\n"
                                               "* This rule also applies to inline code, URLs, file paths, and any other original‑form tokens.\n"
                                               "Avoid literal translations that would sound unnatural or lose meaning."
                                               "You should only output the translated text. \n```{{text}}```")
  "User prompt used by ChatGPT."
  :group 'gptel-translate
  :type 'string)

;;; Faces
(defface gptel-translate-header-desc-face '((t :inherit font-lock-variable-name-face))
  "Used in the buffer's `header-line-format' for description."
  :group 'gptel-translate)

(defface gptel-translate-original-face
  '((((background light)) :background "ivory")
    (((background dark)) :background "gray20" :foreground "gray" :weight light))
  "Used in the buffer's source text."
  :group 'gptel-translate)

(defface gptel-translate-translation-face
  '((t :background "slategray" :foreground "white" :extend t))
  "Face for translated text in translation buffer."
  :group 'gptel-translate)

;;; Prompt

(defun gptel-translate-apply-prompt (prompt replaces)
  "Replace {{key}} placeholders in PROMPT with values from REPLACES.
REPLACES is an alist of (KEY . VALUE) pairs, e.g. (\"to\" . \"Chinese\").
Each {{key}} in PROMPT is replaced with its corresponding VALUE.
Returns the modified prompt string."
  (if replaces
      (let ((result prompt))
        (pcase-dolist (`(,key . ,val) replaces)
          (setq result
                (replace-regexp-in-string
                 (regexp-quote (format "{{%s}}" key))
                 val
                 result
                 t t)))
        result)
    prompt))

;;; Internal helpers

(defun gptel-translate--resolve-backend ()
  "Return the backend to use for translation requests."
  (or (and gptel-translate-backend
           (gptel-get-backend gptel-translate-backend))
      gptel-backend))

(defun gptel-translate--resolve-model ()
  "Return the model symbol to use for translation requests."
  (or gptel-translate-model gptel-model))

(defun gptel-translate--collect-paragraphs (&optional beg end)
  "Collect paragraphs from buffer (or region BEG to END).
Returns a list of paragraph strings in order."
  (let (paragraphs)
    (save-excursion
      (save-restriction
        (when (and beg end) (narrow-to-region beg end))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (forward-paragraph)
            (when (> (point) start)
              (push (buffer-substring-no-properties start (point))
                    paragraphs))
            (skip-chars-forward "\n\t ")))))
    (nreverse paragraphs)))

(defun gptel-translate--make-result-buffer (orig-name paragraphs)
  "Create and return a new buffer for translation results.
ORIG-NAME is the source buffer name.
PARAGRAPHS is a list of original paragraph strings.

Each result entry gets a text property `gptel-translate-slot-marker'
whose value is a marker pointing to the translation insertion point.
Returns the buffer and a list of those markers."
  (let ((buf (generate-new-buffer
              (format "*translate %s*" orig-name)))
        markers)
    (with-current-buffer buf
      (setq header-line-format
            (append
             '(" ")
             (list (propertize "Translation of " 'face 'gptel-translate-header-desc-face)
                   (propertize orig-name 'face 'font-lock-keyword-face)
                   (propertize " " 'face 'gptel-translate-header-desc-face))
             (list (propertize "Total: " 'face 'font-lock-keyword-face)
                   (propertize (format "%d" (length paragraphs)) 'face 'font-lock-keyword-face)
                   (propertize " paragraphs" 'face 'gptel-translate-header-desc-face))))
      (cl-loop for para in paragraphs
               for n from 1
               for slot = (progn
                            (when (> n 1)
                              (insert "\n"))
                            (insert (propertize para 'face 'gptel-translate-original-face))
                            (insert "\n")
                            (point-marker))
               do (push slot markers)
               do (insert "\n"))
      (setq markers (nreverse markers))
      (goto-char (point-min))
      (forward-line 2)
      (read-only-mode 1))
    (cons buf markers)))

(defun gptel-translate--set-translation-status (result-buf slot status)
  "In RESULT-BUF at SLOT (a marker), insert STATUS text.
STATUS can be a translated string, `nil' meaning \"translating...\",
or an error string.  SLOT always points to the start of the
translation insertion area."
  (with-current-buffer result-buf
    (let ((inhibit-read-only t)
          (pos (marker-position slot)))   ; remember the original start
      (save-excursion
        (goto-char pos)
        ;; Delete previous content from pos to next blank line or buffer end
        (let ((end (save-excursion
                     (if (search-forward "\n\n" nil t)
                         (match-beginning 0)
                       (point-max)))))
          (delete-region pos end))
        ;; Insert new content
        (let ((start (point)))
          (insert (cond
                   ((null status) "<translating...>")
                   ((stringp status)
                    (propertize status 'face 'gptel-translate-translation-face))
                   (t (format "<%s>" status))))
          ;; Move the marker back to the start of the inserted text
          (set-marker slot pos))))))

;;; Commands

;;;###autoload
(defun gptel-translate-buffer (&optional beg end)
  "Translate buffer (or region BEG to END) paragraph-by-paragraph.

Show original text and translation side-by-side in a new buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((gptel-backend (gptel-translate--resolve-backend))
         (gptel-model (gptel-translate--resolve-model))
         (gptel-tools nil)
         (gptel-use-tools nil)
         (orig-name (buffer-name))
         (paragraphs (gptel-translate--collect-paragraphs beg end))
         (total (length paragraphs))
         (result-and-slots (gptel-translate--make-result-buffer orig-name paragraphs))
         (result-buf (car result-and-slots))
         (slots (cdr result-and-slots))
         (done 0)
         (failures 0))
    ;; Bail out if nothing to translate
    (when (zerop total)
      (kill-buffer result-buf)
      (message "Nothing to translate")
      (cl-return-from gptel-translate-buffer))
    (display-buffer result-buf)
    ;; Send requests sequentially via recursive callback chain
    (cl-labels ((send-one (idx)
                  (if (>= idx total)
                      (message "Translation complete: %d ok, %d failed, %d total"
                               done failures total)
                    (let* ((para (nth idx paragraphs))
                           (n (1+ idx))
                           (slot (nth idx slots)))
                      (gptel-translate--set-translation-status
                       result-buf slot nil)
                      (message "Translating paragraph %d/%d..." n total)
                      (gptel-request (gptel-translate-apply-prompt gptel-translate-user-prompt
                                                                   `(("to" . ,gptel-translate-target-language)
                                                                     ("text" . ,para)))
                        :system gptel-translate-system-prompt
                        :stream nil
                        :callback
                        (lambda (response _info)
                          (cond ((and (stringp response)
                                      (not (string-empty-p response)))
                                 (progn
                                   (gptel-translate--set-translation-status
                                    result-buf slot response)
                                   (cl-incf done)
                                   (send-one (1+ idx))))
                                ((consp response))
                                ((eq response 'abort)
                                 (message "Translation aborted at paragraph %d" n))
                                (t (progn (gptel-translate--set-translation-status
                                           result-buf slot
                                           (format "<FAILED: %s>"
                                                   (if (null response)
                                                       "no response"
                                                     "error")))
                                          (cl-incf failures)
                                          (send-one (1+ idx)))))))))))
      (send-one 0))))

(provide 'gptel-translate)
;;; gptel-translate.el ends here
