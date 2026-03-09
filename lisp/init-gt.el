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

(setopt gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))

(with-eval-after-load 'gt
  (require 'lib-gt)
  (setopt gt-preset-translators
          `((default . ,(gt-translator
                         :taker   (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if #'gt--taker-paragraph-p)
                                        (gt-taker :text 'sentence :if #'gt--at-line-beginning-p)
                                        (gt-taker :text 'word))
                         :engines (list (gt-google-engine :if '(and not-word))
                                        (gt-bing-engine :if '(and not-word))
                                        (gt-youdao-dict-engine :if '(word))
                                        (gt-youdao-suggest-engine :if '(and word src:en)))
                         :render  (gt-buffer-render)))
            (multi-dict . ,(gt-translator :taker (gt-taker :prompt t)
                                          :engines (list (gt-bing-engine)
                                                         (gt-youdao-dict-engine)
                                                         (gt-youdao-suggest-engine :if 'word)
                                                         (gt-google-engine))
                                          :render (gt-buffer-render)))
            (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                              :render (gt-buffer-render))))))


(autoload #'gt-translate-prompt "lib-gt" nil t)
(autoload #'gt-use-text-utility "lib-gt" nil t)

;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-region-map
    ("t" . gt-translate)))

(global-bind-keys
 ("C-c G"   . gt-translate-prompt)
 ("C-c d t" . gt-translate)
 ("C-c d G" . gt-translate-prompt)
 ("C-c d u" . gt-use-text-utility))

(provide 'init-gt)
;;; init-gt.el ends here
