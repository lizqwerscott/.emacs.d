;;; init-sql.el --- some sql packages                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(sql-indent))

(add-hook 'sql-mode-hook
          #'sqlind-minor-mode)

(provide 'init-sql)
;;; init-sql.el ends here.
