(setq conda-anaconda-home
      (expand-file-name "/opt/anaconda"))
(setq conda-env-home-directory
      (expand-file-name "~/.conda"))

(require 'conda)

(add-hooks '(python-mode python-ts-mode)
           #'pyvenv-mode)

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
