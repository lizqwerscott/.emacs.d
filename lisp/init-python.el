;;; init-python.el --- init python package           -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:
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
;;; init-python.el ends here.
