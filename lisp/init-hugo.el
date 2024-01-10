(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-base-dir
        (file-truename "~/MyProject/website/blog/")))

(provide 'init-hugo)
