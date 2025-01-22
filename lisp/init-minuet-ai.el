;;; init-minuet-ai.el --- init minuet ai package     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

(require 'minuet)

(setenv "DEEPSEEK_API_KEY"
        (gptel-api-key-from-auth-source "api.deepseek.com"))

(add-to-list 'minuet-auto-suggestion-block-functions
             #'(lambda ()
                 (bound-and-true-p meow-normal-mode)))

(keymap-sets minuet-active-mode-map
             '(("TAB" . minuet-accept-suggestion)))

(add-hook 'prog-mode-hook
          #'minuet-auto-suggestion-mode)

(provide 'init-minuet-ai)
;;; init-minuet-ai.el ends here
