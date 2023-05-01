(require 'go-translate)
(setq gts-translate-list '(("en" "zh")))
(setq gts-default-translator
      (gts-translator
       :picker (gts-noprompt-picker)
       :engines (list (gts-bing-engine))
       :render (gts-posframe-pop-render)))
(provide 'init-go-translate)
