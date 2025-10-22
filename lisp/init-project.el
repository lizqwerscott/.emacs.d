;;; init-project.el --- config project package       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott(require 'project) <lizqwerscott@gmail.com>
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

(require 'project)
(require 'project-x)
(require 'projection)

;;; config
(setq project-find-functions '(project-try-vc my/project-try-local))

;;; project-prefix-map
;; (defalias 'project-prefix-map project-prefix-map)

;; (define-key mode-specific-map "p" 'project-prefix-map)
(define-key project-prefix-map (kbd "F") #'project-find-file-other-window)
(define-key project-prefix-map (kbd "b") #'consult-project-buffer)
(define-key project-prefix-map (kbd "B") #'consult-project-buffer-other-window)
(define-key project-prefix-map (kbd "v") #'magit-project-status)

;;; project-switch-commands
(setq project-switch-commands nil)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(project-find-file-other-window "Find file ow") t)
(add-to-list 'project-switch-commands '(consult-project-buffer "switch to buffer") t)
(add-to-list 'project-switch-commands '(consult-project-buffer-other-window "switch to buffer ow") t)
(add-to-list 'project-switch-commands '(magit-project-status "Git Status") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find Dir") t)
(add-to-list 'project-switch-commands '(project-dired "Dired") t)
(add-to-list 'project-switch-commands '(project-find-regexp "Regexp") t)

;;; Menu
(require 'transient)
(transient-define-prefix project-manage-dispatch ()
  "Manage Project menu"
  [["Manage"
    ("r" "Forget under" project-forget-projects-under)
    ("z" "Forget zombie" project-forget-zombie-projects)
    ("a" "Remember under" project-remember-projects-under)]]
  [("q" "Quit" transient-quit-one)])


(transient-define-prefix project-dispatch ()
  "Project dispatch menu"
  [
   :class transient-row
   :description "Project"
   ("C-p" "Manage" project-manage-dispatch
    :transient t)
   ("p" "Switch" project-switch-project)
   ("P" "Switch Open" project-switch-project-open)]
  [["Find"
    ("f" "File" project-find-file)
    ("F" "File OW" project-find-file-other-window)
    ("o" "Other file" projection-find-other-file)
    ("d" "Dir" project-dired-dir)
    ("D" "Dir Fuzzy" consult-project-fd-dir)
    ("g" "Regexp" project-find-regexp)]
   ["Buffer"
    ("b" "Switch" consult-project-buffer)
    ("B" "Switch OW" consult-project-buffer-other-window)
    ("k" "Kill" project-kill-buffers)]
   ["Build"
    ("c c" "Compile" projection-commands-build-project)
    ("c r" "Run" projection-commands-run-project)
    ("c t" "Test" projection-commands-test-project)
    ("m" "Mutli compile" projection-multi-compile)]
   ["Dir Locals"
    ("e e" "Edit" project-edit-dir-local)
    ("e s" "Trust" project-add-to-safe-local-variable)
    ("e a" "Add" project-add-dir-local-variable)]
   ["Other"
    ("v" "Magit status" unpackaged/magit-project-status)
    ("r" "Rsync" rsync-project-dispatch)
    ("t" "Vterm" multi-vterm-project)
    ("s""Eshell"
     (lambda ()
       (interactive)
       (autoload 'eshell-project-toggle "init-eshell" nil t)
       (eshell-project-toggle)))]]
  [("q" "Quit" transient-quit-all)])

(provide 'init-project)
;;; init-project.el ends heres.
