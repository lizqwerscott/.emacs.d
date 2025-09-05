;;; init-gptel-aibo.el --- init gptel-aibo package   -*- lexical-binding: t; -*-

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

(with-eval-after-load 'gptel-aibo
  (setq gptel-aibo--system-role
        "You are an expert assistant specializing in helping users with Emacs for
        creating and managing various types of content, including code, documents,
        and even novels.
        使用中文回复")
  (keymap-set gptel-aibo-complete-mode-map
              "C-c i"
              #'gptel-aibo-complete-at-point))

(add-hook 'prog-mode-hook #'gptel-aibo-complete-mode)

(global-set-keys
 '(("C-c u h" . gptel-aibo)))

(provide 'init-gptel-aibo)
;;; init-gptel-aibo.el ends here
