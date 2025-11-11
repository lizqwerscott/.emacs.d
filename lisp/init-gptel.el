;;; init-gptel.el --- init gptel package             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

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

(setq gptel-default-mode 'org-mode)

(gptel-make-openai "kimi"
  :stream t
  :protocol "https"
  :host "api.moonshot.cn"
  :key #'gptel-api-key
  :models '(moonshot-v1-8k))

(gptel-make-openai "fireworks"
  :stream t
  :protocol "https"
  :host "api.fireworks.ai"
  :endpoint "/inference/v1/chat/completions"
  :key #'gptel-api-key
  :models '(accounts/fireworks/models/llama-v3p1-405b-instruct))

(gptel-make-deepseek "deepseek"
  :stream t
  :key #'gptel-api-key)

(gptel-make-anthropic "Claude"
  :stream t
  :host "api.openai-proxy.org/anthropic"
  :key #'gptel-api-key)

(gptel-make-openai "Closeai"
  :stream t
  :host "api.openai-proxy.org"
  :key #'gptel-api-key
  :models '(deepseek-chat deepseek-reasoner gpt-4o-mini))

(gptel-make-openai "OpenRouter"
  :stream t
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :key #'gptel-api-key
  :models '(openrouter/auto google/gemini-2.0-flash-lite-preview-02-05:free deepseek/deepseek-r1:free deepseek/deepseek-chat:free))

(gptel-make-openai "Aliyun"
  :stream t
  :protocol "https"
  :host "dashscope.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :key #'gptel-api-key
  :models '(qwen3-coder-480b-a35b-instruct qwen3-coder-plus))

(gptel-make-openai "ModelScope"
  :stream t
  :protocol "https"
  :host "api-inference.modelscope.cn"
  :key #'gptel-api-key
  :models '((Qwen/Qwen3-Coder-480B-A35B-Instruct
             :description "这款模型在代理式编程、浏览器操作等任务上表现接近 Claude Sonnet，具备最高可扩展至百万级别的长上下文理解能力，并支持跨平台的代理式编程与特定函数调用格式。"
             :capabilities (tool-use)
             :context-window 200
             :request-params (:temperature 0.7 :top_p 0.8 :top_k 20 :repetition_penalty 1.05))))

(setq gptel-model 'deepseek-chat)
(setq gptel-backend
      (gptel-get-backend "deepseek"))

(require 'gptel)

(require 'prompts)
(setq prompt-templates
      (get-all-prompts
       (concat user-emacs-directory
               "config/prompts")))

(add-list-to-list 'gptel-directives
                  `((translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                          "  Follow my instructions and improve or rewrite the text I provide."
                                          "  Generate ONLY the replacement text,"
                                          " without any explanation or markdown code fences or org code fences."
                                          " translate chinese to english."))
                    (docstr . ,(make-prompt (alist-get 'docstr prompt-templates) nil))
                    (emacs . ,(make-prompt (alist-get 'emacs prompt-templates) nil))
                    (ragmacs . ,(make-prompt (alist-get 'ragmacs prompt-templates) nil))
                    (translate-english . ,(make-prompt (alist-get 'translate prompt-templates)
                                                       '(("to" . "english")
                                                         ("user_name" . "lizqwerscott"))))))

(require 'init-gptel-tools)

(defun gptel-translate-to-english (&optional dry-run)
  "Use AI to translate the currently selected text into English.

The DRY-RUN parameter is set to t, indicating that it will not actually run, but only simulate the run."
  (interactive "P")
  (let (((gptel-tools nil)
         (gptel-use-tools nil)))
    (gptel-request (list (or (get-char-property (point) 'gptel-rewrite)
                             (buffer-substring-no-properties (region-beginning) (region-end)))
                         "What is the required change?"
                         "Rewrite:")
      :dry-run dry-run
      :system (alist-get 'translate gptel-directives)
      :stream t
      :context
      (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
                    (make-overlay (region-beginning) (region-end) nil t))))
        (overlay-put ov 'category 'gptel)
        (overlay-put ov 'evaporate t)
        (cons ov (generate-new-buffer "*gptel-rewrite*")))
      :callback #'gptel--rewrite-callback)))

(defun gptel-translate-to-english-insert (str)
  "Use AI to translate the STR into English."
  (interactive (list (read-string "Input: " nil nil nil default-input-method)))
  (let ((buffer (current-buffer))
        (pos (point))
        (gptel-tools nil)
        (gptel-use-tools nil))
    (gptel-request str
      :dry-run nil
      :system (alist-get 'translate-english gptel-directives)
      :stream t
      :callback (lambda (res info)
                  (message "res: %s" res)
                  (with-current-buffer buffer
                    (save-excursion
                      (goto-char pos)
                      (cond
                       ((stringp res)
                        (insert res))
                       ((eq res 'abort)
                        (message "error"))
                       ((null res)
                        (message "LLM respone error"))
                       ((consp res))
                       (t
                        (message "Translated finish")))
                      (setq pos (point))))))))

(with-eval-after-load 'gptel-transient
  (transient-append-suffix 'gptel-menu '(2 -1)
    ["Quick Tools"
     ("q t" "Translate select regions to english" gptel-translate-to-english)]))

(defun gptel--create-buffer (buffer-name prompt)
  "Create or switch to a GPT session buffer and initialize it.  BUFFER-NAME is the name of the target buffer.  PROMPT is the prompt text to insert during initialization.

This function performs the following operations:
1. Create or switch to the buffer with the given name
2. Set the major mode according to =gptel-default-mode'
3. Validate and sanitize model configuration using =gptel--sanitize-model'
4. Insert the prompt text at the end of the buffer
5. Display the buffer using =gptel-display-buffer-action'
6. Enable =gptel-mode' if not already active
7. Send the initial request via =gptel-send'

If the buffer is empty, the prompt is prefixed with \"*** \".  If the buffer already contains content, the prompt is appended at the end."
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

(defun gptel-global-chat (message)
  "Global chat with MESSAGE."
  (interactive (list (read-string (format "%s\nInput: "
                                          (propertize "@emacs 可以读取 Emacs 文档\n@elisp-document 用来写 elisp 函数的文档\n@ragmacs 支持读取源码和变量"
                                                      'face 'font-lock-comment-face))
                                  (if (and (use-region-p)
                                           (equal major-mode 'emacs-lisp-mode))
                                      "@emacs "
                                    "")
                                  nil nil
                                  t)))
  (let ((gptel-display-buffer-action '(nil
                                       (display-buffer-reuse-mode-window display-buffer-at-bottom)
                                       (body-function . select-window)))
        (buffer-name "*global-chat*")
        (prompt (format "%s%s"
                        message
                        (if (use-region-p)
                            (format ": %s"
                                    (buffer-substring (region-beginning)
                                                      (region-end)))
                          ""))))
    (gptel--create-buffer buffer-name prompt)))

(defun gptel-project-chat ()
  "Global chat in project."
  (interactive)
  (when-let* ((project (project-current))
              (name (project-name project))
              (root-dir (file-truename (project-root project)))
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
              (prompt (format "%s%s"
                              message
                              (if (use-region-p)
                                  (format ": %s"
                                          (buffer-substring (region-beginning)
                                                            (region-end)))
                                ""))))
    (gptel--create-buffer buffer-name prompt)))

;;; preset
(gptel-make-preset 'default
  :description "Default"
  :system (alist-get 'default gptel-directives)
  :tools nil
  :use-tools nil)

(gptel-make-preset 'emacs
  :description "Emacs 大师"
  :system (alist-get 'emacs gptel-directives)
  :tools '("read_documentation")
  :use-tools t)

(gptel-make-preset 'elisp-document
  :description "Elisp 文档大师"
  :system (alist-get 'docstr gptel-directives)
  :tools '("read_documentation")
  :use-tools t)

;; ragmacs
(gptel-make-preset 'ragmacs
  :descr "Ragmacs"
  :pre (lambda () (require 'ragmacs))
  :system (alist-get 'ragmacs gptel-directives)
  :tools '("introspection")
  :use-tools t)

(global-bind-keys
 ("C-c RET" . gptel-send)
 ("C-c u g" . gptel)
 ("C-c u G" . gptel-menu)
 ("C-c u c" . gptel-global-chat)
 ("C-c u p" . gptel-project-chat))

(add-hook 'gptel-mode-hook
          #'gptel-highlight-mode)

(require 'init-gptel-aibo)

(add-hook 'magit-mode-hook #'gptel-magit-install)

(with-eval-after-load 'gptel-magit
  (defun gptel-no-tool-wrap (orig-fn &rest args)
    "Let ORIG-FN not use tools.
ARGS is ORIG-FN args."
    (let ((gptel-tools nil)
          (gptel-use-tools nil))
      (apply orig-fn args)))

  (advice-add #'gptel-magit-commit-generate :around #'gptel-no-tool-wrap))


(provide 'init-gptel)
;;; init-gptel.el ends here
