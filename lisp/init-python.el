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

(defun run-python-file ()
  "Run now python file."
  (interactive)
  (autoload 'project-root-path "init-project" nil t)
  (let* ((project-path (project-root-path))
         (command (concat (if project-path
                              (cond ((file-exists-p (file-name-concat project-path "uv.lock")) "uv run")
                                    ((file-exists-p (file-name-concat project-path "pdm.lock")) "pdm run")
                                    (t "python"))
                            "python")
                          " "
                          (file-truename (buffer-file-name)))))
    (setq command (compilation-read-command command))
    (require 'multi-vterm)
    (multi-vterm-run command)))

(keymap-sets (python-mode-map python-ts-mode-map)
  '(("C-c r" . run-python-file)))

(provide 'init-python)
;;; init-python.el ends here
