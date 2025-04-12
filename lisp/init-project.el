(require 'project)
(require 'projection)

(global-projection-hook-mode)

;;; config
(setq xref-search-program 'ripgrep)

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

;;; project-prefix-map
;; (defalias 'project-prefix-map project-prefix-map)

;; (define-key mode-specific-map "p" 'project-prefix-map)

;; (with-eval-after-load "project"
;;   (define-key project-prefix-map "b" #'consult-project-buffer)
;;   (define-key project-prefix-map "s" #'shell)
;;   (define-key project-prefix-map "t" #'find-temp-project))

;; (define-key project-prefix-map (kbd "b") #'consult-project-buffer)
;; (define-key project-prefix-map (kbd "s") #'shell)
;; (define-key project-prefix-map (kbd "t") #'find-temp-project)
(define-key project-prefix-map (kbd "v") #'magit-project-status)

;;; project-switch-commands
(setq project-switch-commands nil)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(magit-project-status "Git Status") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find Dir") t)
(add-to-list 'project-switch-commands '(project-dired "Dired") t)

;;; Menu
(pretty-hydra-define-e hydra-project
  (:title "Project" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Basic"
   (("f" project-find-file "find file")
    ("o" projection-find-other-file "find other file")
    ("d" project-dired "dired")
    ("D" project-dired-dir "dired")
    ("b" consult-project-buffer "buffer")
    ("k" project-kill-buffers "kill buffers"))
   "project"
   (("p" project-switch-project "switch other")
    ("r" project-forget-project "forget")
    ("a" project-remember-project "remember")
    ("c" rsync-project-dispatch "rsync"))
   "Other"
   (("v" magit-project-status "git")
    ("t" multi-vterm-project "vterm")
    ("e" eshell-project-toggle "eshell"))))

(provide 'init-project)
;;; init-project.el ends heres.
