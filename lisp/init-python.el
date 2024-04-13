(setq conda-anaconda-home
      (expand-file-name "/opt/anaconda"))
(setq conda-env-home-directory
      (expand-file-name "~/.conda"))

(require 'conda)

(add-hooks '(python-mode python-ts-mode)
           #'pyvenv-mode)

(provide 'init-python)
