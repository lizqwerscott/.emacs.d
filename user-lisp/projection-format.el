;;; projection-format.el --- Whole-project formatting via projection types  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "28.1") (projection "0.4.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A lightweight, type-driven whole-project formatter for `projection'.
;; The projection type name (e.g. `rust-cargo', `npm', `cmake') is used
;; as a key into `projection-format-commands'; individual projects can
;; override the command via .dir-locals.el.

;;; Code:

(require 'projection-core)

(defgroup projection-format nil
  "Whole-project formatting driven by projection types."
  :group 'projection
  :prefix "projection-format-")

(defcustom projection-format-commands
  '((rust-cargo . "cargo fmt"))
  "Alist mapping projection type name to a whole-project format command.

The key is the `name' slot of the EIEIO object returned by
`projection-project-type', e.g. `rust-cargo', `npm', `cmake'.
The value is either a shell command string or a function of no
arguments called with `default-directory' bound to the project root."
  :type '(alist :key-type symbol
                :value-type (choice string function))
  :group 'projection-format)

(defcustom projection-format-command nil
  "Format command for the current project, overriding `projection-format-commands'.

Meant to be set per-project in .dir-locals.el:

  ((nil . ((projection-format-command . \"ruff format .\"))))"
  :type '(choice (const :tag "Use type default" nil)
                 string
                 function)
  :safe (lambda (v) (or (null v) (stringp v) (functionp v)))
  :group 'projection-format)

(defun projection-format--type-name (root)
  "Return the projection type name of the project at ROOT, or nil."
  (when-let* ((type (projection-project-type root)))
    (oref type name)))

(defun projection-format--compile (command)
  "Run COMMAND asynchronously, isolated from the build/test compilation buffer.

Uses `compile' so output still supports `next-error' / `compilation-mode',
but binds `compilation-buffer-name-function' so the formatter does not
clobber the `*compilation*' buffer used by `project-compile' and
`projection-commands-*-project'."
  (let ((compilation-buffer-name-function
         (lambda (_mode) "*projection-format*")))
    (compile command)))

;;;###autoload
(defun projection-format-project ()
  "Format the whole current project.

Resolution order:
  1. `projection-format-command' (dir-locals override)
  2. `projection-format-commands' keyed by the projection type name
  3. signal an error"
  (interactive)
  (let* ((pr        (project-current t))
         (root      (project-root pr))
         (type-name (projection-format--type-name root))
         (cmd       (or projection-format-command
                        (alist-get type-name projection-format-commands)
                        (user-error
                         "No format command for projection type %s; \
set `projection-format-command' in .dir-locals.el or extend `projection-format-commands'"
                         type-name)))
         (default-directory root))
    (save-some-buffers
     t (lambda ()
         (when-let ((bpr (project-current)))
           (equal (project-root bpr) root))))
    (pcase cmd
      ((pred functionp) (funcall cmd))
      ((pred stringp)   (projection-format--compile cmd))
      (_ (user-error "Invalid format command: %S" cmd)))))

(provide 'projection-format)
;;; projection-format.el ends here
