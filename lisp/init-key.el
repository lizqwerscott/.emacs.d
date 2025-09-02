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
   ("C-x C-b" . ibuffer-refersh)))

(keymap-unset ctl-x-map "C-k")

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

(defun consult-fd-in-home ()
  (interactive)
  (consult-fd "~"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in line in buffer" :command consult-line)
 (:key "L" :description "Search in line in multi buffer" :command consult-line-multi)
 (:key "b" :description "Bookmark" :command consult-bookmark)
 (:key "j" :description "Color rg search symbol" :command color-rg-search-input :filename "init-color-rg")
 (:key "p" :description "Color rg search symbol in project" :command color-rg-search-input-in-project :filename "init-color-rg")
 (:key "r" :description "rg menu" :command rg-menu :filename "init-rg")
 (:key "g" :description "Webjump" :command webjump :filename "init-webjump")
 (:key "f" :description "Search file in home" :command consult-fd-in-home)
 (:key "y" :description "Search YASsnippet" :command consult-yasnippet)
 (:key "w" :description "Search in web" :command consult-omni-multi :filename "init-consult-omni"))

(defun ibuffer-refersh ()
  (interactive)
  (when-let* ((buffer (get-buffer "*Ibuffer*")))
    (kill-buffer buffer))
  (ibuffer))

(lazy-one-key-create-menu
 "Buffer"
 (:key "b" :description "Switch buffer" :command consult-buffer)
 (:key "B" :description "Switch buffer other window" :command consult-buffer-other-window)
 (:key "s" :description "ibuffer" :command ibuffer-refersh)
 (:key "k" :description "Kill buffer" :command kill-buffer-and-window)
 (:key "T" :description "Switch telega buffers" :command telega-switch-buffer :filename "init-telega")
 (:key "i" :description "Switch telega important chat" :command telega-switch-important-chat :filename "init-telega")
 (:key "t" :description "Switch telega chat" :command telega-chat-with :filename "init-telega")
 (:key "r" :description "Revert buffer" :command revert-buffer-quick)
 (:key "h" :description "bury buffer" :command bury-buffer)
 (:key "l" :description "unbury buffer" :command unbury-buffer))

(lazy-one-key-create-menu
 "FileAction"
 (:key "c" :description "Copy File name" :command +lizqwer/copy-file-name-to-clipboard :filename "init-func")
 (:key "C" :description "Copy File Path" :command +lizqwer/copy-file-path-to-clipboard :filename "init-func")
 (:key "d" :description "Delete this file" :command bufferfile-delete :filename "init-bufferfile")
 (:key "r" :description "Rename this file" :command bufferfile-rename :filename "init-bufferfile")
 (:key "b" :description "Browse this file" :command browse-this-file :filename "init-func"))

(one-key-create-menu
 "File"
 '((("f" . "Find file") . find-file)
   (("o" . "Find other file") . ff-find-other-file)
   (("F" . "Find file other window") . find-file-other-window)
   (("s" . "Fuzzy search") . consult-fd)
   (("a" . "Action file") . one-key-menu-fileaction)
   (("r" . "Recent file") . consult-recent-file)
   (("h" . "Find in main dir") . (lambda ()
                                   (interactive)
                                   (ido-find-file-in-dir "~/")))
   (("p" . "Find in project") . (lambda ()
                                  (interactive)
                                  (require 'project)
                                  (ido-find-file-in-dir "~/MyProject")))
   (("c" . "Find custom file") . find-custom-file)))

(lazy-one-key-create-menu
 "Useful"
 (:key "t" :description "Sdcv translate" :command sdcv-search-pointer+ :filename "init-sdcv")
 (:key "S" :description "Sudo edit find file" :command sudo-edit-find-file :filename "sudo-edit")
 (:key "e" :description "Toggle sdcv" :command lsp-bridge-toggle-sdcv-helper :filename "init-lsp-bridge")
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
