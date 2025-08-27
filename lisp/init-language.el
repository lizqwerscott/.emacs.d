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

(pretty-hydra-define-e hydra-language
  (:title "Language" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("dict"
   (("s" sdcv-search-pointer+ "sdcv dict")
    ("f" fanyi-dwim2 "Fanyi Point")
    ("F" fanyi-dwim "Fanyi Input"))
   "english"
   (("t" gt-translate "translate")
    ("e" gptel-translate-to-english-insert "Translate to english"))))

(provide 'init-language)
;;; init-language.el ends here
