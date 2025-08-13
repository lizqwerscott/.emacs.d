;;; init-language.el --- init language package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; gt
(require 'init-gt)

;;; fanyi
(custom-set-variables
 '(fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-longman-provider)))


(provide 'init-language)
;;; init-language.el ends here
