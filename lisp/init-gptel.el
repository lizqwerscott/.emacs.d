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

;; (setq gptel-model "gpt-3.5-turbo")
(setq gptel-model "accounts/fireworks/models/llama-v3p1-405b-instruct")

(setq gptel-backend
      (gptel-make-openai "chinese"
        :stream t
        :protocol "https"
        :host "api.fireworks.ai"
        :endpoint "/inference/v1/chat/completions"
        :key #'gptel-api-key
        :models '("accounts/fireworks/models/llama-v3p1-405b-instruct")))

(require 'gptel)
(add-list-to-list 'gptel-directives
                  '((translate . "你是一个生活在 Emacs 里面的大语言模型, 一可以翻译英语到中文, 也能将中文翻译到英文.")))

(global-set-key (kbd "C-c RET") #'gptel-send)

(provide 'init-gptel)
;;; init-gptel.el ends here
