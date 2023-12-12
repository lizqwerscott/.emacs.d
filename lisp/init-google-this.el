(require 'google-this)

(cl-defun google-this-with-prefix (&optional (prefix ""))
  (interactive "P")
  (google-this-string
   nil
   (concat prefix
           (cond
            ((region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
            ((thing-at-point 'symbol) (thing-at-point 'symbol))
            ((thing-at-point 'word) (thing-at-point 'word))
            (t (buffer-substring (line-beginning-position) (line-end-position)))))))

(one-key-create-menu
 "Google"
 '((("g" . "With null") . google-this)
   (("e" . "With emacs") . (lambda ()
                             (interactive)
                             (google-this-with-prefix "emacs ")))))
(provide 'init-google-this)
