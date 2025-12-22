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

(with-eval-after-load 'project
  (require 'project-x)
  (require 'projection)

  (add-hook 'project-find-functions #'my/project-try-local)
  (add-hook 'project-find-functions #'rc/find-root-for-eglot-for-clj))

;;; Menu
(transient-define-prefix project-manage-dispatch ()
  "Manage Project menu"
  ["Project"
   :description (lambda ()
                  (let* ((project (project-current)))
                    (if project
                        (format "Project: %s" (nth 2 project))
                      "Project")))
   ["Manage"
    ("r" "Forget under" project-forget-projects-under)
    ("z" "Forget zombie" project-forget-zombie-projects)
    ("a" "Remember under" project-remember-projects-under)]]
  [("q" "Quit" transient-quit-one)])

;;; project-prefix-map
(defvar-keymap project-compile-map
  :doc "Project compile map."
  :prefix t
  "c" '("Compile" . projection-commands-build-project)
  "r" '("Run" . projection-commands-run-project)
  "t" '("Test" . projection-commands-test-project))

(keymap-set project-prefix-map "c" '("Compile" . project-compile-map))

(defvar-keymap project-dir-locals-map
  :doc "Project dir-locals map."
  :prefix t
  "e" '("Edit" . project-edit-dir-local)
  "s" '("Trust" . project-add-to-safe-local-variable)
  "a" '("Add" . project-add-dir-local-variable))

(keymap-set project-prefix-map "e" '("Dir Locals" . project-dir-locals-map))

(keymap-unset project-prefix-map "C-b")
(keymap-binds project-prefix-map
  ("F" . project-find-file-other-window)
  ("o" . projection-find-other-file)

  ("b" . consult-project-buffer)
  ("B" . consult-project-buffer-other-window)

  ("m" . ("Multi compile" . projection-multi-compile))
  ("R" . ("Rsync" . rsync-project-dispatch))
  ("t" . ("Vterm" . multi-vterm-project))
  ("s" . ("Eshell" . project-eshell))
  ("v" . unpackaged/magit-project-status)

  ("M" . project-manage-dispatch)
  ("P" . project-switch-project-open)

  ("C" . ("Emacs Config" . (lambda () (interactive) (project-switch-project user-emacs-directory)))))

;;; project-switch-commands
(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-regexp "Find regexp")
        (project-find-dir "Find Dir")
        (project-dired "Dired")
        (unpackaged/magit-project-status "Git")
        (consult-project-buffer "Switch buffer")
        (project-find-file-other-window "Find file(OW)")
        (consult-project-buffer-other-window "Switch buffer(OW)")))

(provide 'init-project)
;;; init-project.el ends heres.
