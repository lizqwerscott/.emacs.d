(require 'webjump)

(global-set-key (kbd "C-c /") #'webjump)

(setq webjump-sites
      '(;; Emacs
        ("Emacs Home Page" .
         "www.gnu.org/software/emacs/emacs.html")
        ("Xah Emacs Site" . "ergoemacs.org/index.html")
        ("(or emacs irrelevant)" . "oremacs.com")
        ("Mastering Emacs" .
         "https://www.masteringemacs.org/")

        ;; Search engines.
        ("Ai" .
         [simple-query "devv.ai"
                       "devv.ai/zh/search/" ""])

        ("DuckDuckGo" .
         [simple-query "duckduckgo.com"
                       "duckduckgo.com/?q=" ""])
        ("Google" .
         [simple-query "www.google.com"
                       "www.google.com/search?q=" ""])
        ("Bing" .
         [simple-query "www.bing.com"
                       "www.bing.com/search?q=" ""])

        ("Baidu" .
         [simple-query "www.baidu.com"
                       "www.baidu.com/s?wd=" ""])
        ("Wikipedia" .
         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])))

(provide 'init-webjump)
