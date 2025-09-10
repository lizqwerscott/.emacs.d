;;; init-key.el --- binding key                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-unset ctl-x-map "b")
(keymap-unset ctl-x-map "C-k")

(global-set-keys
 '(("RET" . newline-and-indent)
   ("S-<return>" . comment-indent-new-line)
   (("s-o" "M-o") . ace-window)
   (("s-n" "M-n") . scroll-up-1/3)
   (("s-p" "M-p") . scroll-down-1/3)
   (("M-N" "s-N") . scroll-other-window-up-1/3)
   (("M-P" "s-P") . scroll-other-window-down-1/3)
   (("s-x" "M-x") . execute-extended-command)
   ("C-s-f" . forward-sexp)
   ("C-s-b" . backward-sexp)

   ("C-x C-r" . consult-recent-file)

   ("C-x b g" . revert-buffer-quick)))

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

(with-eval-after-load 'compile
  (keymap-sets compilation-mode-map
    '(("s-n" . compilation-next-error)
      ("s-p" . compilation-previous-error))))

;;; bufferfile
(lazy-load-global-keys
 '(("C-x b r" . bufferfile-rename)
   ("C-x b k" . bufferfile-delete)
   ("C-x K" . bufferfile-delete))
 "init-bufferfile")

;;; rg
(lazy-load-global-keys
 '(("M-s s" . rg-dwim)
   ("M-s R" . rg-menu)
   ("M-s r" . rg)
   ("M-s t" . rg-literal)
   ("M-s p" . rg-project))
 "init-rg")

;;; symbol overlay
(lazy-load-global-keys
 '(("M-i" . symbol-overlay-put)
   ("s-i" . symbol-overlay-put))
 "symbol-overlay")

;;; yank
(global-set-key (kbd "M-y") #'consult-yank-pop)

;;; gif screenshot
(with-eval-after-load 'gif-screencast
  (keymap-sets gif-screencast-mode-map
    '(("<f8>" . gif-screencast-toggle-pause)
      ("<f9>" . gif-screencast-stop))))

;;; vterm
(with-eval-after-load 'vterm
  (keymap-sets vterm-mode-map
    '(("C-y" . vterm-yank))))

(provide 'init-key)
;;; init-key.el ends here
