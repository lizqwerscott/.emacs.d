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

;;; Projection Type
(defvar projection-project-type-python-uv
  (projection-type
   :name 'python-uv
   :predicate (defun projection-python-uv-project-p ()
                (and (file-exists-p "pyproject.toml")
                     (file-exists-p "uv.lock")))
   :build "uv build"
   :run (defun projection-python-uv-project-run-command ()
          (concat "uv run " (file-truename (buffer-file-name))))))

(add-to-list 'projection-project-types projection-project-type-python-uv)


;;; project-prefix-map
;; (defalias 'project-prefix-map project-prefix-map)

;; (define-key mode-specific-map "p" 'project-prefix-map)
(define-key project-prefix-map (kbd "B") #'project-switch-to-buffer-other-window)
(define-key project-prefix-map (kbd "v") #'magit-project-status)

;;; project-switch-commands
(setq project-switch-commands nil)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(project-switch-to-buffer "switch to buffer") t)
(add-to-list 'project-switch-commands '(project-switch-to-buffer-other-window "switch to buffer other window") t)
(add-to-list 'project-switch-commands '(magit-project-status "Git Status") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find Dir") t)
(add-to-list 'project-switch-commands '(project-dired "Dired") t)

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
    ("D" "Dir Fuzzy" consult-project-fd-dir)]
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
    ("e a" "Add" add-dir-local-variable)]
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

(pretty-hydra-define-e hydra-project
  (:title "Project" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("project"
   (("p" project-switch-project "Switch project")
    ("P" project-switch-project-open "Switch to open project")
    ("C-p r" project-forget-projects-under "Forget under")
    ("C-p z" project-forget-zombie-projects "Forget zombie")
    ("C-p a" project-remember-projects-under "Remember under"))
   "Find"
   (("f" project-find-file "Find file")
    ("F" project-find-file-other-window "Find file in other window")
    ("o" projection-find-other-file "other file")
    ("d" project-dired-dir "Find dir in project root")
    ("D" project-dired "Open project root dir"))
   "Buffer"
   (("b" consult-project-buffer "Switch buffer")
    ("B" consult-project-buffer-other-window "Switch buffer other window")
    ("k" project-kill-buffers "Kill buffers"))
   "Build"
   (("c c" projection-commands-build-project "Compile")
    ("c r" projection-commands-run-project "Run")
    ("c t" projection-commands-test-project "Test")
    ("m" projection-multi-compile "Mutli compile"))
   "Dir Locals"
   (("e e" project-edit-dir-local "Edit")
    ("e s" project-add-to-safe-local-variable "Trust")
    ("e a" add-dir-local-variable "Add"))
   "Other"
   (("v" magit-project-status "Magit status")
    ("r" rsync-project-dispatch "Rsync")
    ("t" multi-vterm-project "Vterm")
    ("s" (lambda ()
           (interactive)
           (autoload 'eshell-project-toggle "init-eshell" nil t)
           (eshell-project-toggle))
     "Eshell"))))

(provide 'init-project)
;;; init-project.el ends heres.
