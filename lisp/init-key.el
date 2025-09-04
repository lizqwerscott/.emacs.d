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

(defun lsp-diagnostic ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'consult-flycheck))
    ('lsp-bridge
     (autoload #'one-key-menu-diagnostic "init-lsp-bridge" nil t)
     (call-interactively #'one-key-menu-diagnostic))))

(defun lsp-rename ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-rename))
    ('lsp-bridge
     (autoload #'lsp-bridge-rename "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-rename))))

(defun lsp-restart ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-shutdown)
     (call-interactively #'eglot))
    ('lsp-bridge
     (autoload #'lsp-bridge-restart-process "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-restart-process))))

(defun lsp-code-action ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-code-actions))
    ('lsp-bridge
     (autoload #'lsp-bridge-code-action "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-code-action))))

(defun lsp-search-symbol ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'consult-eglot-symbols))
    ('lsp-bridge
     (autoload #'lsp-bridge-workspace-list-symbols "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-workspace-list-symbols))))

(defun code-peek ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (if (bound-and-true-p citre-mode)
         (call-interactively #'citre-peek)
       (message "Eglot not support peek")))
    ('lsp-bridge
     (autoload #'lsp-bridge-peek "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-peek))))

(lazy-one-key-create-menu
 "Code"
 (:key "f" :description "Format code" :command format-code-buffer :filename "init-format")
 (:key "d" :description "Diagnostic" :command lsp-diagnostic)
 (:key "r" :description "Lsp rename" :command lsp-rename)
 (:key "R" :description "Lsp restart" :command lsp-restart)
 (:key "a" :description "Lsp code action" :command lsp-code-action)
 (:key "s" :description "Lsp search symbol" :command lsp-search-symbol)
 (:key "p" :description "Code peek" :command code-peek))

(lazy-one-key-create-menu
 "Org"
 (:key "w" :description "Open org file" :command open-my-org-file :filename "init-org")
 (:key "c" :description "Open org capture" :command org-capture :filename "init-org")
 (:key "a" :description "Open org agenda" :command one-key-menu-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org")
 (:key "s" :description "Org search" :command consult-notes)
 (:key "r" :description "Org roam" :command one-key-menu-roam)
 (:key "t" :description "Vterm" :command multi-vterm-open :filename "multi-vterm")
 (:key "m" :description "Media note" :command org-media-note-show-interface))

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
