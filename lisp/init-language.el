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

(global-bind-keys
 ("C-c d f" . fanyi-dwim)
 ("C-c d d" . fanyi-dwim2)
 ("C-c d h" . fanyi-from-history)
 ("C-c d s" . sdcv-search-pointer+)
 ("C-c d e" . ("Translate to english" . (lambda ()
                                          (interactive)
                                          (activate-input-method default-input-method)
                                          (call-interactively #'gptel-translate-to-english-insert)))))

;;; spell
(require 'init-spell)

;;; input
(require 'init-pyim)
(require 'init-rime)

(setq default-input-method "rime")

(provide 'init-language)
;;; init-language.el ends here
