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

(setq gptel-backend-kimi
      (gptel-make-openai "kimi"
        :stream t
        :protocol "https"
        :host "api.moonshot.cn"
        :key #'gptel-api-key
        :models '(moonshot-v1-8k)))

(setq gptel-backend-fireworks
      (gptel-make-openai "chinese"
        :stream t
        :protocol "https"
        :host "api.fireworks.ai"
        :endpoint "/inference/v1/chat/completions"
        :key #'gptel-api-key
        :models '(accounts/fireworks/models/llama-v3p1-405b-instruct)))

(setq gptel-backend-deepseek
      (gptel-make-openai "deepseek"
        :stream t
        :protocol "https"
        :host "api.deepseek.com"
        :key #'gptel-api-key
        :models '(deepseek-chat)))

;; (setq gptel-model 'moonshot-v1-8k)
;; (setq gptel-backend gptel-backend-kimi)
(setq gptel-model 'deepseek-chat)
(setq gptel-backend gptel-backend-deepseek)

(require 'gptel)
(add-list-to-list 'gptel-directives
                  `((translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                          "  Follow my instructions and improve or rewrite the text I provide."
                                          "  Generate ONLY the replacement text,"
                                          " without any explanation or markdown code fences or org code fences."
                                          " Rewrite: 将当前文本翻译到英语"))))
(setq gptel-use-tools t)
(require 'init-gptel-tools)

(global-set-key (kbd "C-c RET") #'gptel-send)

(require 'gptel-quick)
(setq gptel-quick-system-message
      #'(lambda (count)
          (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
                 (article (if (and lang (not (string-empty-p lang))
                                 (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                              "an" "a")))
            (if (derived-mode-p 'prog-mode)
                (format (concat "You are %s %s programmer.  "
                                "Explain in %d words or fewer."
                                "It is best to use Chinese for the explanation.")
                        article lang count)
              (concat
               (if (string-empty-p lang)
                   "You are an editor."
                 (format "You are %s %s editor." article lang))
               (format "Explain in %d words or fewer." count)
               "It is best to use Chinese for the explanation.")))))

(global-set-keys
 '((("M-?" "s-?") . gptel-quick)))

(provide 'init-gptel)
;;; init-gptel.el ends here
