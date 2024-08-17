;;; init-rime.el --- init emacs rime package         -*- lexical-binding: t; -*-

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

(require 'rime)
(setq rime-user-data-dir "~/.local/share/fcitx5/rime/")

(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p
        pyim-probe-meow-normal-mode
        rime-predicate-hydra-p
        rime-predicate-ace-window-p
        rime-predicate-in-code-string-p
        rime-predicate-punctuation-after-space-cc-p
        rime-predicate-space-after-cc-p))

(require 'im-cursor-chg)
(cursor-chg-mode t)

(provide 'init-rime)
;;; init-rime.el ends here
