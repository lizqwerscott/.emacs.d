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
  :models '(qwen3-coder-480b-a35b-instruct))

(setq gptel-model 'qwen3-coder-480b-a35b-instruct)
;; (setq gptel-model 'deepseek-reasoner)
(setq gptel-backend
      (gptel-get-backend "Aliyun"))
;; (setq gptel-model 'claude-3-5-sonnet-20241022)
;; (setq gptel-backend
;;       (gptel-get-backend "Claude"))

(require 'gptel)
(add-list-to-list 'gptel-directives
                  `((translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                          "  Follow my instructions and improve or rewrite the text I provide."
                                          "  Generate ONLY the replacement text,"
                                          " without any explanation or markdown code fences or org code fences."
                                          " translate chinese to english."))))

(require 'init-gptel-tools)

(defun gptel-translate-to-english (&optional dry-run)
  "Use AI to translate the currently selected text into English."
  (interactive "P")
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
    :callback #'gptel--rewrite-callback))

(with-eval-after-load 'gptel-transient
  (transient-append-suffix 'gptel-menu '(2 -1)
    ["Quick Tools"
     ("q t" "Translate select regions to english" gptel-translate-to-english)]))

(global-set-key (kbd "C-c RET") #'gptel-send)

(require 'init-gptel-aibo)

(provide 'init-gptel)
;;; init-gptel.el ends here
