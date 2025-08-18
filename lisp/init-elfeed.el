(require 'elfeed)

(setq elfeed-feeds
      '(("https://emacstalk.codeberg.page/podcast/index.xml" blog emacs)
        ("https://manateelazycat.github.io/feed.xml" blog)
        ("https://remacs.fun/index.xml" blog emacs)
        ("https://whatacold.io/zh-cn/rss.xml" blog)
        ("http://feed.williamlong.info/" blog)
        ("https://sachachua.com/blog/category/emacs-news/feed" emacs)))

(setf url-queue-timeout 30)

(defun nerd-icon-for-tags (tags)
  "Generate Nerd Font icon based on tags.
  Returns default if no match."
  (cond ((member "star" tags) (nerd-icons-faicon "nf-fa-star" :face '(:foreground "#FFD700")))
        ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
        (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

(defun lucius/elfeed-search-print-entry--better-default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                           (elfeed-entry-title entry) "")
                        ;; NOTE: insert " " for overlay to swallow
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))


         ;; Title/Feed ALIGNMENT
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when feed-title
      (insert " " (concat (nerd-icon-for-tags tags) " ")
              (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))

(setq elfeed-search-print-entry-function #'lucius/elfeed-search-print-entry--better-default)

(keymap-sets elfeed-show-mode-map
  '(("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)))

(provide 'init-elfeed)
