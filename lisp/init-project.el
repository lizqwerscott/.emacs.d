(require 'project)
(require 'projection)

;;; config
(defun my/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".project")
                      ("go.mod" "Cargo.toml" "pom.xml") ;; higher priority
                      ("Makefile"))))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let* ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root))))))))

(setq project-find-functions '(project-try-vc my/project-try-local))

(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))

(cl-defmethod project-files (project &optional dirs)
  "Override `project-files' to use `fd' and Check if the current project is a single file project in transient(eglot) projects."
  (if (equal 'transient (car project))
      (progn
        (message "eglot transient single file")
        (when-let* ((server (eglot-current-server))
                    (buffers (eglot--managed-buffers server))
                    (paths (project--remote-file-names
                            (mapcar #'(lambda (buffer)
                                        (file-truename (buffer-file-name buffer)))
                                    buffers))))
          paths))
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project))))))

;;; Functions
;;;###autoload
(defun project-root-path ()
  "Get current project path"
  (let ((project (project-current nil)))
    (when project
      (project-root project))))

(defun my/project-info ()
  (interactive)
  (message "%s" (project-current t)))

(defun my/add-dot-project ()
  (interactive)
  (let* ((root-dir (read-directory-name "Root: "))
         (f (expand-file-name ".project" root-dir)))
    (message "Create %s..." f)
    (make-empty-file f)))

(defun my/project-discover ()
  "Add dir under search-path to project."
  (interactive)
  (dolist (search-path '("~/MyProject/" "~/github/"))
    (dolist (file (file-name-all-completions  "" search-path))
      (when (not (member file '("./" "../")))
        (let ((full-name (expand-file-name file search-path)))
          (when (file-directory-p full-name)
            (when-let* ((pr (project-current nil full-name)))
              (project-remember-project pr)
              (message "add project %s..." pr))))))))

(defun c++-generate-class (name)
  "Generate c++ class.
NAME is class name."
  (interactive "sClass name:")
  (let ((generate-path (read-directory-name "make class directory:"
                                            (project-root (project-current))))
        (auto-generate-command "python ~/MyProject/AutoGenerate/main.py"))
    (shell-command (concat auto-generate-command
                           " "
                           (concat "--name "
                                   name)
                           (concat " --path "
                                   generate-path)))))

(defun c++-generate-class-and-dir (name)
  "Generate c++ class and create class directory.
NAME is class name."
  (interactive "sClass name:")
  (let ((generate-path (read-directory-name "make class directory:"
                                            (project-root (project-current))))

        (auto-generate-command "python ~/MyProject/AutoGenerate/main.py"))
    (let ((dir-path (concat generate-path
                            name
                            "/")))
      (make-directory dir-path)
      (shell-command (concat auto-generate-command
                             " "
                             (concat "--name "
                                     name)
                             (concat " --path "
                                     dir-path))))))

(defun project-dired-dir (dired-dir)
  (interactive (list
                (read-directory-name "Dired open: " (project-root (project-current)))))
  (dired dired-dir))

(defun project--open-projects ()
  "Return a list of projects with open buffers."
  (let* ((buffer-list
          ;; Ignore ephemeral buffers
          (match-buffers (lambda (buf)
                           (not (string-prefix-p " " (buffer-name buf))))))
         (directories
          (cl-remove-duplicates (mapcar
                                 (lambda (buf)
                                   (buffer-local-value 'default-directory buf))
                                 buffer-list)
                                :test #'equal)))
    (cl-remove-duplicates
     (seq-mapcat (lambda (directory)
                   (if-let* ((project (project-current nil directory)))
                       (list project)))
                 directories)
     :test (lambda (p1 p2) (equal (project-root p1) (project-root p2))))))

(defun project-switch-project-open ()
  "Switch to an open project to dispatch commands on."
  (interactive)
  (let* ((open-projects (mapcar #'project-root (project--open-projects)))
         ;; `project--file-completion-table' seems to accept any collection as
         ;; defined by `completing-read'.
         (completion-table (project--file-completion-table open-projects))
         (project-directory (completing-read "Select open project: "
                                             completion-table nil t)))
    (project-switch-project project-directory)))

(defun project-find-file-in-other-window (suggested-filename dirs project &optional include-all)
  "Complete a file name in DIRS in PROJECT and visit the result.

SUGGESTED-FILENAME is a file name, or part of it, which
is used as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files from DIRS, except for VCS
directories listed in `vc-directory-exclusion-list'."
  (let* ((vc-dirs-ignores (mapcar
                           (lambda (dir)
                             (concat dir "/"))
                           vc-directory-exclusion-list))
         (all-files
          (if include-all
              (mapcan
               (lambda (dir) (project--files-in-directory dir vc-dirs-ignores))
               dirs)
            (project-files project dirs)))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (default-directory (project-root project))
         (file (project--read-file-name
                project "Find file"
                all-files nil 'file-name-history
                suggested-filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (find-file-other-window file))))

(defun project-find-file-other-window (&optional include-all)
  "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (dirs (list root))
         (project-files-relative-names t))
    (project-find-file-in-other-window
     (delq nil (list (and buffer-file-name (project--find-default-from
                                            buffer-file-name pr))
                     (thing-at-point 'filename)))
     dirs pr include-all)))

(defun project-switch-to-buffer-other-window (buffer-or-name)
  "Display buffer BUFFER-OR-NAME in the other window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical."
  (interactive (list (project--read-project-buffer)))
  (switch-to-buffer-other-window buffer-or-name))

(defun project-edit-dir-local ()
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (dir-locals-file (concat default-directory
                                  ".dir-locals.el")))
    (if (file-exists-p dir-locals-file)
        (find-file dir-locals-file)
      (call-interactively #'add-dir-local-variable))))

(defun project-add-to-safe-local-variable ()
  (interactive)
  (let* ((project-path (project-root (project-current))))
    (customize-save-variable
     'safe-local-variable-directories
     (add-to-list 'safe-local-variable-directories
                  (file-truename project-path)))))

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
