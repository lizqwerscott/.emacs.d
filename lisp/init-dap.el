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

(pretty-hydra-define hydra-dape (:title (pretty-hydra-title "Debug" 'codicon "nf-cod-debug") :color pink :quit-key ("q" "C-g"))
  ("Stepping"
   (("n" dape-next "next")
    ("s" dape-step-in "step in")
    ("o" dape-step-out "step out")
    ("c" dape-continue "continue")
    ("p" dape-pause "pause")
    ;; ("k" dape-kill "kill")
    ("r" dape-restart "restart")
    ("D" dape-disconnect-quit "disconnect"))
   "Switch"
   (("i" dape-info "info")
    ("R" dape-repl "repl")
    ("m" dape-read-memory "memory")
    ("t" dape-select-thread "thread")
    ("S" dape-select-stack "stack")
    ("w" dape-watch-dwim "memory"))
   "Breakpoints"
   (("b" dape-breakpoint-toggle "toggle")
    ;; ("l" dape-breakpoint-log "log")
    ("a" dape-breakpoint-log "log")
    ("e" dape-breakpoint-expression "expression")
    ("B" dape-breakpoint-remove-all "clear"))
   "Debug"
   (("d" dape "dape")
    ("Q" #'(lambda ()
             (interactive)
             (dape-quit)
             (stop-posframe)) "quit" :exit t))))
;; Save buffers on startup, useful for interpreted languages
(add-hook 'dape-on-start-hooks
          (defun dape--save-on-start ()
            (save-some-buffers t t)))

(defun start-dape ()
  (interactive)
  (start-posframe)
  (hydra-dape/body))

;; Display hydra on startup
(add-hook 'dape-on-start-hooks
          #'start-dape)

(global-set-key (kbd "<f5>") #'start-dape)

(provide 'init-dap)
;;; init-dap.el ends here
