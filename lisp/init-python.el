(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home
        (expand-file-name "/opt/anaconda"))
  (setq conda-env-home-directory
        (expand-file-name "~/.conda")))

;;; Format
(use-package python-black
  :ensure t)

(provide 'init-python)
