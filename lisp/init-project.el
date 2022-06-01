(require 'project)

;configure project.el
(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))

(defun generate-cmake-ccls ()
  "Generated cmake for ccls (support cmake project)."
  (interactive)
  (let ((root-project (project-root (project-current))))
    (shell-command
     (concat (concat "cd " root-project)
             "&& cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"))
    (when (not (file-exists-p
                (concat root-project
                        "compile_commands.json")))
      (shell-command
       (concat "ln -s ./Debug/compile_commands.json "
               root-project))))
  (message "cmake ccls finish"))

(defvar temp-file-dir "~/temp/" "set default temp file dir.")

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
Support Lisp, python, c++."
  (interactive (list (completing-read "choose one create:"
                                      `(("Lisp" . 1)
                                        ("Python" . 2)
                                        ("C++" . 3))
                                      nil t "Lisp")))
  (let ((project-name (read-directory-name "make project directory name:"
                                           temp-file-dir)))
    (make-directory project-name)
    (cond ((string= args "Lisp") (make-lisp-temp project-name))
          ((string= args "Python") (make-python-temp project-name))
          ((string= args "C++") (make-c++-temp project-name))
          (t (error "in make-temp-file, the args get another.(%s)" args))))
  (message args))

(defun find-temp-project ()
  "Find temp project"
  (interactive "")
  (find-file (read-directory-name "find temp project:"
                                  temp-file-dir)))

(provide 'init-project)
