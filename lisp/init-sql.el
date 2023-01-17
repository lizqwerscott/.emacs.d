;;; init-sql.el --- some sql packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:
;;; Code:

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

(provide 'init-sql)
;;; init-sql.el ends here.
