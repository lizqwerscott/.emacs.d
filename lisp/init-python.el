;;; init-python.el --- config python                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(conda pyvenv))

(setq conda-anaconda-home
      (expand-file-name "/opt/anaconda"))
(setq conda-env-home-directory
      (expand-file-name "~/.conda"))

(require 'conda)

(add-hooks '(python-mode python-ts-mode)
           #'pyvenv-mode)

;;; eglot
(with-eval-after-load 'eglot
  (defun random-hex-string (n)
    "Generate random N len hex string."
    (let ((str ""))
      (dotimes (_ n str)
        (setq str (format "%s%02x" str (random 256))))))

  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(lambda (_interactive _project)
                                                   (list "basedpyright-langserver"
                                                         "--stdio"
                                                         (format "--cancellationReceive=file:%s"
                                                                 (random-hex-string 21))))))

  (setq-default eglot-workspace-configuration
                '(:basedpyright (:typeCheckingMode "basic"))))

;;; Projection
(with-eval-after-load 'init-project
  (defvar projection-project-type-python-uv
    (projection-type
     :name 'python-uv
     :predicate (defun projection-python-uv-project-p ()
                  (and (file-exists-p "pyproject.toml")
                       (file-exists-p "uv.lock")))
     :build "uv build"
     :run (defun projection-python-uv-project-run-command ()
            (concat "uv run " (file-truename (buffer-file-name))))))

  (add-to-list 'projection-project-types projection-project-type-python-uv))

(require 'project-x)

(defun setting-python-compile-command ()
  "Setting python default `compile-command'."
  (let* ((project-path (project-root-path))
         (command (concat (if project-path
                              (cond ((file-exists-p (file-name-concat project-path "uv.lock")) "uv run")
                                    ((file-exists-p (file-name-concat project-path "pdm.lock")) "pdm run")
                                    (t "python"))
                            "python")
                          " "
                          (file-truename (buffer-file-name)))))
    (setq-local compile-command
                command)))

(add-hooks '(python-mode python-ts-mode)
           #'setting-python-compile-command)

(keymap-binds (python-mode-map python-ts-mode-map)
  ("C-c r" . project-run-command-with-vterm))

(provide 'init-python)
;;; init-python.el ends here
