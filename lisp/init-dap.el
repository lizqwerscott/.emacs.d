;;; init-dap.el --- init dape                        -*- lexical-binding: t; -*-

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

(require 'dape)

(custom-set-variables
 '(dape-buffer-window-arrangment 'right))

(require 'transient)
(transient-define-prefix dape-dispatch ()
  "Dape dispatch menu."
  :transient-non-suffix 'transient--do-stay
  [["Stepping"
    ("s" "step in" dape-step-in :transient t)
    ("o" "step out" dape-step-out :transient t)
    ("c" "continue" dape-continue :transient t)
    ("p" "pause" dape-pause :transient t)
    ("r" "restart" dape-restart :transient t)
    ("D" "disconnect" dape-disconnect-quit :transient t)]
   ["Switch"
    ("i" "info" dape-info :transient t)
    ("R" "repl" dape-repl :transient t)
    ("m" "memory" dape-memory :transient t)
    ("t" "thread" dape-select-thread :transient t)
    ("S" "stack" dape-select-stack :transient t)
    ("w" "watch" dape-watch-dwim :transient t)]
   ["Breakpoints"
    ("b" "toggle" dape-breakpoint-toggle :transient t)
    ("a" "log" dape-breakpoint-log :transient t)
    ("e" "expression" dape-breakpoint-expression :transient t)
    ("B" "clear" dape-breakpoint-remove-all :transient t)]
   ["Debug"
    ("d" "dape" dape :transient t)
    ("Q" "quit" dape-quit)]]
  [("q" "Done" transient-quit-all)])

(defun dape--save-on-start ()
  "Dape save buffers on start."
  (save-some-buffers t t))

(add-hook 'dape-start-hook #'dape--save-on-start)

;; Display Dispatch on startup
(add-hook 'dape-start-hook
          #'dape-dispatch)

(global-set-key (kbd "<f5>") #'dape-dispatch)

(provide 'init-dap)
;;; init-dap.el ends here
