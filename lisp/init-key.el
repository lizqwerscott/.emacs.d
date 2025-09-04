;;; init-key.el --- binding key                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-unset ctl-x-map "b")
(keymap-unset ctl-x-map "C-k")

(defun ibuffer-refersh ()
  (interactive)
  (when-let* ((buffer (get-buffer "*Ibuffer*")))
    (kill-buffer buffer))
  (ibuffer))

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
   ("C-x C-b" . ibuffer-refersh)

   ("C-x b g" . revert-buffer-quick)))

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

(with-eval-after-load 'compile
  (keymap-sets compilation-mode-map
    '(("s-n" . compilation-next-error)
      ("s-p" . compilation-previous-error))))

;; ###autoload
(defmacro lazy-one-key-create-menu (title &rest keybinds)
  (let (one-key-key-alist)
    (dolist (ele keybinds)
      (when (plist-get ele :filename)
        (autoload (plist-get ele :command) (plist-get ele :filename) nil t))
      (push
       (if (plist-get ele :run)
           (cons (cons (plist-get ele :key) (plist-get ele :description)) (plist-get ele :run))
         (cons (cons (plist-get ele :key) (plist-get ele :description)) (plist-get ele :command)))
       one-key-key-alist))
    `(one-key-create-menu ,title (quote ,one-key-key-alist))))

(lazy-one-key-create-menu
 "Useful"
 (:key "S" :description "Sudo edit find file" :command sudo-edit-find-file :filename "sudo-edit")
 (:key "D" :description "Docker" :command docker)
 (:key "p" :description "peek code" :command peek-overlay-dwim :filename "init-peek")
 (:key "d" :description "Devdocs" :command devdocs-lookup)
 (:key "s" :description "screenshot" :command screenshot)
 (:key "c" :description "Insert color" :command my-insert-color-hex :filename "init-func")
 (:key "g" :description "gptel" :command gptel)
 (:key "G" :description "gptel menu" :command gptel-menu)
 (:key "h" :description "gptel aibo" :command gptel-aibo)
 (:key "a" :description "Aider" :command aidermacs-transient-menu))

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

(provide 'init-key)
;;; init-key.el ends here
