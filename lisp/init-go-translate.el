;;; init-go-translate.el --- init go translate package  -*- lexical-binding: t; -*-

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

(require 'go-translate)
(setq gt-langs '(en zh))

(setq gt-default-translator
      (gt-translator
       ;; :taker   (gt-taker :text 'buffer :pick 'paragraph)       ; 配置拾取器
       :engines (list (gt-bing-engine)) ; 指定多引擎
       :render  (gt-buffer-render)))                            ; 配置渲染器


(provide 'init-go-translate)
;;; init-go-translate.el ends here
