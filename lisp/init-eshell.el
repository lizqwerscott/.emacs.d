;;; init-eshell.el --- eshell                        -*- lexical-binding: t; -*-

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

;;; eshell prompt
(with-eval-after-load 'eshell
  (require 'em-dirs)

  (defun my/eshell-prompt-day ()
    "Day Theme prompt."
    (concat
     (propertize "🦊 " 'face '(:foreground "#8B4513"))                       ;; 图标 - 深棕色
     (propertize (user-login-name) 'face '(:foreground "#191970"))           ;; 用户名 - 藏青色
     (propertize "@" 'face '(:foreground "#505050"))                         ;; 分隔符 - 深灰色
     (propertize (file-name-nondirectory (eshell/pwd))                       ;; 目录名 - 深绿色
                 'face '(:foreground "#006400" :bold t :weight 'bold))
     (propertize "/ > " 'face '(:foreground "#4B0082"))))

  (defun my/eshell-prompt-night ()
    "Night Theme prompt."
    (let* ((faces '(font-lock-keyword-face    ; 图标
                    font-lock-function-name-face ; 用户名
                    font-lock-comment-face    ; 分隔符
                    font-lock-string-face     ; 目录名
                    font-lock-type-face))     ; 提示符
           (colors (mapcar (lambda (face)
                             (face-attribute face :foreground nil 'default))
                           faces)))
      (concat
       (propertize "🦊 " 'face `(:foreground ,(nth 0 colors)))
       (propertize (user-login-name) 'face `(:foreground ,(nth 1 colors)))
       (propertize "@" 'face `(:foreground ,(nth 2 colors)))
       (propertize (file-name-nondirectory (eshell/pwd))
                   'face `(:foreground ,(nth 3 colors) :bold t))
       (propertize " > " 'face `(:foreground ,(nth 4 colors)))))))

(setopt eshell-prompt-function #'my/eshell-prompt-night)

(with-eval-after-load 'esh-mode
  (eshell-syntax-highlighting-global-mode +1))

;;; eshell `eldoc' support
(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'esh-help)
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function
                  'esh-help-eldoc-command)))

(defun eshell/clear-buffer ()
  "Clear the eshell buffer."
  (interactive)
  (recenter-top-bottom t))

;;; keymap
(with-eval-after-load 'eshell
  (keymap-binds eshell-mode-map
    ("C-l" . eshell/clear-buffer)
    (("M-s" "s-s" "M-r" "s-r") . consult-history)
    ("s-p" . eshell-previous-input)
    ("s-n" . eshell-next-input)))

(provide 'init-eshell)
;;; init-eshell.el ends here
