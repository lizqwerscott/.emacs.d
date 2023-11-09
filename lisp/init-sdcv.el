(require 'sdcv)
(setq sdcv-say-word-p nil)               ;say word after translation

(setq sdcv-dictionary-data-dir (file-truename "~/.emacs.d/config/dic/")) ;setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"
        "朗道英汉字典5.0"))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
        "懒虫简明英汉词典"
        "英汉汉英专业词典"
        "KDic11万英汉词典"
        "朗道英汉字典5.0"))

(provide 'init-sdcv)
