;;; lib-gptel.el --- lib gptel                       -*- lexical-binding: t; -*-

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

(require 'project)
(require 'which-func)

(require 'gptel)

(defun gptel--create-buffer (buffer-name prompt)
  "Create or switch to a GPT session buffer and initialize it.

BUFFER-NAME is the name of the target buffer.
PROMPT is the prompt text to insert during initialization.

This function performs the following operations:
1. Create or switch to the buffer with the given name
2. Set the major mode according to =gptel-default-mode'
3. Validate and sanitize model configuration using =gptel--sanitize-model'
4. Insert the prompt text at the end of the buffer
5. Display the buffer using =gptel-display-buffer-action'
6. Enable =gptel-mode' if not already active
7. Send the initial request via =gptel-send'

If the buffer is empty, the prompt is prefixed with \"*** \". If the buffer
already contains content, the prompt is appended at the end."
  (with-current-buffer (get-buffer-create buffer-name)
    (cond                               ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (gptel--sanitize-model :backend (default-value 'gptel-backend)
                           :model (default-value 'gptel-model)
                           :shoosh nil)
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (if (bobp)
        (insert (concat "*** " prompt))
      (goto-char (point-max))
      (insert prompt))
    (display-buffer (current-buffer) gptel-display-buffer-action)
    (unless gptel-mode (gptel-mode 1))
    (gptel-send)))

(defun my/get-surrounding-chars-pos-with-cursor (&optional n-chars)
  "Get content with BUFFER cursor.
N-CHARS is max size."
  (let* ((n (or n-chars 500))
         (pt (point))
         (buf-min (point-min))
         (buf-max (point-max))
         (start-pos (max buf-min (- pt n)))
         (end-pos   (min buf-max (+ pt n))))
    (cons start-pos end-pos)))

(defun my/cursor-symbol ()
  "Get cursor thing."
  (when-let* ((thing (or (thing-at-point 'url)
                         (thing-at-point 'existing-filename)
                         (thing-at-point 'filename)
                         (thing-at-point 'symbol)))
              (thing (substring-no-properties thing)))
    thing))

(declare-function breadcrumb-imenu-crumbs "breadcrumb")

(defun my/cursor-function ()
  "Get cursor function."
  (if (not (featurep 'breadcrumb))
      (which-function)
    (when-let* ((imenu-crumbs (breadcrumb-imenu-crumbs))
                (func (substring-no-properties imenu-crumbs)))
      func)))

(defun my/cursor-prompt ()
  "Get cursor prompt."
  (let* ((thing (my/cursor-symbol))
         (function (my/cursor-function))
         (prompts (list (if thing
                            (format "Cursor point (%s)" thing)
                          "")
                        (if function
                            (format "Cursor function (%s)" function)
                          ""))))
    (when prompts
      (format "[%s]"
              (string-join prompts "|")))))

(defun my/cursor-content ()
  "Get cursor content."
  (when-let* ((bounds (if (use-region-p)
                          (cons (region-beginning)
                                (region-end))
                        (my/get-surrounding-chars-pos-with-cursor))))
    (list (current-buffer) :bounds (list bounds))))

(defun gptel-global-chat ()
  "Global chat."
  (interactive)
  (let* ((gptel-display-buffer-action '(nil
                                        (display-buffer-reuse-mode-window display-buffer-at-bottom)
                                        (body-function . select-window)))
         (buffer-name "*global-chat*")
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (comp)
               (and-let* ((desc
                           (plist-get (gptel-get-preset (intern-soft comp))
                                      :description)))
                 (concat (propertize " " 'display '(space :align-to 32))
                         (if (string-match "\\(\n\\)" desc)
                             (substring desc 0 (match-beginning 1))
                           desc))))))
         (preset (completing-read "Select main preset: "
                                  '((emacs)
                                    (elisp-document)
                                    (ragmacs)
                                    (python-program))
                                  nil t))
         (message (read-string (format "%s\nInput: @%s "
                                       (propertize "主 Preset:\n@emacs 可以读取 Emacs 文档\n@elisp-document 用来写 elisp 函数的文档\n@ragmacs 支持读取源码和变量\n附加 Preset:\n@file-search 可以用来搜索文件"
                                                   'face 'font-lock-comment-face)
                                       preset)
                               nil
                               nil nil
                               t))
         (prompt (format "@%s %s%s"
                         preset
                         message
                         (concat " " (my/cursor-prompt))))
         (gptel-context (append gptel-context (list (my/cursor-content)))))
    (gptel--create-buffer buffer-name prompt)))

(defun gptel-project-chat ()
  "Global chat in project."
  (interactive)
  (when-let* ((project (project-current))
              (name (project-name project))
              (root-dir (file-truename (project-root project)))
              (completion-extra-properties
               `(:annotation-function
                 ,(lambda (comp)
                    (and-let* ((desc
                                (plist-get (gptel-get-preset (intern-soft comp))
                                           :description)))
                      (concat (propertize " " 'display '(space :align-to 32))
                              (if (string-match "\\(\n\\)" desc)
                                  (substring desc 0 (match-beginning 1))
                                desc))))))
              (preset (completing-read "Select Project preset: "
                                       '((macher)
                                         (macher-ro)
                                         (macher-notools))
                                       nil t))
              (message (read-string (format "%s\nInput: "
                                            (propertize "@macher 可以读写文件全部工具\n@macher-ro 只能读取文件\n@macher-notools 没有工具"
                                                        'face 'font-lock-comment-face))
                                    "@macher "
                                    nil nil
                                    t))
              (gptel-display-buffer-action '(nil
                                             (display-buffer-reuse-mode-window display-buffer-at-bottom)
                                             (body-function . select-window)))
              (buffer-name (format "*%s-chat %s*" name root-dir))
              (prompt (format "@%s %s%s"
                              preset
                              message
                              (concat " " (my/cursor-prompt))))
              (gptel-context (append gptel-context (list (my/cursor-content)))))
    (gptel--create-buffer buffer-name prompt)))

(provide 'lib-gptel)
;;; lib-gptel.el ends here
