;;; init-sql.el --- some sql packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:
;;; Code:

(add-hook 'sql-mode-hook
          #'sqlind-minor-mode)

(provide 'init-sql)
;;; init-sql.el ends here.
