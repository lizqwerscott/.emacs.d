;;; init-language.el --- init language package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; gt
(require 'init-gt)

;;; immersive-translate

(setopt immersive-translate-backend 'trans)

;;; fanyi
(custom-set-variables
 '(fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-longman-provider)))

;;; dict
(setopt quick-sdcv-dictionary-prefix-symbol "►"
        quick-sdcv-ellipsis " ▼"
        quick-sdcv-dictionary-data-dir (expand-file-name "config/dict" user-emacs-directory))

(add-hook 'quick-sdcv-mode-hook #'goto-address-mode)

(global-bind-keys
 ("C-c d f" . fanyi-dwim)
 ("C-c d d" . fanyi-dwim2)
 ("C-c d h" . fanyi-from-history)
 ("C-c d s" . quick-sdcv-search-at-point)
 ("C-c d i" . quick-sdcv-search-input)
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
