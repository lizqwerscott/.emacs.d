(require 'project)
(require 'projection)

(require 'project-x)

;;; config
(setq project-find-functions '(project-try-vc my/project-try-local))

;;; Find Temp project
(defvar temp-file-dir "~/temp/" "Set default temp file dir.")

(defun make-lisp-temp (name)
  (let ((file-name (concat name "/main.lisp")))
    (make-empty-file file-name)
    (find-file file-name)))

(defun make-python-temp (name)
  (let ((file-name (concat name "/main.py")))
    (make-empty-file file-name)
    (find-file file-name)))

(defun make-c++-temp (name)
  (let ((file-name (concat name "/main.cpp")))
    (make-empty-file file-name)
    (find-file file-name)))

(defun make-temp-project (args)
  "Make temp file.
ARGS is temp project name
Support Lisp, python, c++."
  (interactive (list (completing-read "choose one create:"
                                      `(("Lisp" . 1)
                                        ("Python" . 2)
                                        ("C++" . 3))
                                      nil t "")))
  (let ((project-name (read-directory-name "make project directory name:"
                                           temp-file-dir)))
    (make-directory project-name)
    (cond ((string= args "Lisp") (make-lisp-temp project-name))
          ((string= args "Python") (make-python-temp project-name))
          ((string= args "C++") (make-c++-temp project-name))
          (t (error "in make-temp-file, the args get another.(%s)" args))))
  (message args))

(defun find-temp-project ()
  "Find temp project."
  (interactive "")
  (find-file (read-directory-name "find temp project:"
                                  temp-file-dir)))

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
   (("b" project-switch-to-buffer "Switch buffer")
    ("B" project-switch-to-buffer-other-window "Switch buffer other window")
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
