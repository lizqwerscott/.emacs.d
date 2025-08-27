;;; init-gt.el --- init gt package                   -*- lexical-binding: t; -*-

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

(require 'gt)
(setq gt-langs '(en zh))

(setq gt-default-translator
      (gt-translator
       :engines
       (list (gt-google-engine :if '(and not-word))
             (gt-bing-engine :if '(and not-word))
             (gt-youdao-dict-engine :if '(word))
             (gt-youdao-suggest-engine :if '(and word src:en)))
       :render  (gt-buffer-render)))

(provide 'init-gt)
;;; init-gt.el ends here
