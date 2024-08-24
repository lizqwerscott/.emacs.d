(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)
(global-set-key (kbd "M-o") #'ace-window)

;;;###autoload
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
 "Toggle"
 (:key "t" :description "Toggle telega" :command +lizqwer/toggle-telega :filename "init-telega")
 ;; (:key "c" :description "Toggle copilot" :command +lizqwer/toggle-copilot :filename "init-copilot")
 (:key "v" :description "Toggle vterm" :command multi-vterm-dedicated-toggle :filename "init-vterm")
 (:key "l" :description "Lock screen" :command +lizqwer/toggle-lock :filename "init-func")
 (:key "b" :description "Toggle big screen mode" :command open-big-screen-mode :filename "init-big-screen")
 (:key "T" :description "Toggle transparent" :command +lizqwer/toggle-transparent :filename "init-func")
 (:key "e" :description "Toggle dark theme" :command +lizqwer/toggle-dark-theme :filename "init-func")
 (:key "p" :description "Toggle proxy" :command +lizqwer/toggle-proxy :filename "init-func")
 (:key "r" :description "Toggle redacted" :command redacted-mode)
 (:key "c" :description "Toggle center cursor" :command global-centered-cursor-mode)
 (:key "s" :description "Toggle interaction-log" :command interaction-log-mode :filename "interaction-log")
 (:key "w" :description "Toggle sub word or super word" :command toggle-sub-word-or-super-word :filename "init-edit")
 (:key "i" :description "Toggle immersive-translate" :command immersive-translate-auto-mode :filename "init-immersive-translate"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in buffer" :command consult-line)
 (:key "i" :description "Search imenu" :command consult-imenu)
 (:key "m" :description "Search imenu int multi buffer" :command consult-imenu-multi)
 (:key "o" :description "Search outline" :command consult-outline)
 (:key "B" :description "Bookmark" :command consult-bookmark)
 (:key "s" :description "Blink Search" :command blink-search :filename "init-blink-search")
 (:key "j" :description "color rg search symbol in project" :command color-rg-search-input-in-project :filename "color-rg")
 (:key "b" :description "Google this" :command one-key-menu-google :filename "init-google-this"))

(lazy-one-key-create-menu
 "Buffer"
 (:key "b" :description "Switch buffer" :command consult-buffer)
 (:key "k" :description "Kill buffer" :commit kill-buffer-and-window)
 (:key "T" :description "Switch telega buffers" :command telega-switch-buffer :filename "init-telega")
 (:key "i" :description "Switch telega important chat" :command telega-switch-important-chat :filename "init-telega")
 (:key "t" :description "Switch telega chat" :command telega-chat-with :filename "init-telega")
 (:key "r" :description "Revert buffer" :command revert-buffer))

(lazy-one-key-create-menu
 "FileAction"
 (:key "c" :description "Copy File name" :command +lizqwer/copy-file-name-to-clipboard :filename "init-func")
 (:key "C" :description "Copy File Path" :command +lizqwer/copy-file-path-to-clipboard :filename "init-func")
 (:key "d" :description "Delete this file" :command delete-this-file :filename "init-func")
 (:key "r" :description "Rename this file" :command rename-this-file :filename "init-func")
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
                                  (ido-find-file-in-dir "~/MyProject")))))


(defmacro open-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(one-key-create-menu
 "Directory"
 `((("h" . "Home Dir") . ,(open-dir "~/"))
   (("c" . "Config Dir") . ,(open-dir "~/.emacs.d/"))
   (("g" . "Github Dir") . ,(open-dir "~/github/"))
   (("p" . "Project Dir") . ,(open-dir "~/MyProject/"))
   (("d" . "Document Dir") . ,(open-dir "~/Documents/"))
   (("s" . "Fuzzy search Dir") . consult-fd-dir)
   (("j" . "Dired jump") . dired-jump)
   (("J" . "Dired jump other window") . dired-jump-other-window)))

(one-key-create-menu
 "sort-tab"
 '((("n" . "Sort tab select next tab") . sort-tab-select-next-tab)
   (("p" . "Sort tab select prev tab") . sort-tab-select-prev-tab)
   (("c" . "Sort tab close current tab") . sort-tab-close-current-tab)
   (("m" . "Sort tab close all mode tabs") . sort-tab-close-mode-tabs)))

(one-key-create-menu
 "Git"
 '((("n" . "Next hunk") . diff-hl-next-hunk)
   (("p" . "Previous hunk") . diff-hl-previous-hunk)
   (("s" . "Show hunk") . diff-hl-show-hunk)
   (("b" . "Blame") . magit-blame)))

(lazy-one-key-create-menu
 "Useful"
 (:key "t" :description "Sdcv translate" :command sdcv-search-pointer+ :filename "init-sdcv")
 (:key "S" :description "Sudo edit find file" :command sudo-edit-find-file :filename "sudo-edit")
 (:key "e" :description "Toggle sdcv" :command lsp-bridge-toggle-sdcv-helper :filename "init-lsp-bridge")
 (:key "D" :description "Docker" :command docker)
 (:key "p" :description "peek code" :command peek-overlay-dwim :filename "init-peek")
 (:key "d" :description "Devdocs" :command devdocs-lookup)
 (:key "s" :description "screenshot" :command screenshot)
 (:key "c" :description "Insert color" :command my-insert-color-hex :filename "init-func"))

(lazy-one-key-create-menu
 "Code"
 (:key "h" :description "Show document" :command lsp-bridge-popup-documentation :filename "init-lsp-bridge")
 (:key "j" :description "Scroll doc up" :command lsp-bridge-popup-documentation-scroll-up :filename "init-lsp-bridge")
 (:key "k" :description "Scroll doc down" :command lsp-bridge-popup-documentation-scroll-down :filename "init-lsp-bridge")
 (:key "f" :description "Format code" :command format-code :filename "init-format")
 (:key "d" :description "Diagnostic" :command one-key-menu-diagnostic :filename "init-lsp-bridge")
 (:key "D" :description "Lsp Bridge jump to def other window" :command lsp-bridge-find-def-other-window :filename "init-lsp-bridge")
 (:key "r" :description "Lsp Bridge rename" :command lsp-bridge-rename :filename "init-lsp-bridge")
 (:key "i" :description "Lsp Bridge find impl" :command lsp-bridge-find-impl :filename "init-lsp-bridge")
 (:key "I" :description "Lsp Bridge find impl other window" :command lsp-bridge-find-impl-other-window :filename "init-lsp-bridge")
 (:key "a" :description "Lsp Bridge code action" :command lsp-bridge-code-action :filename "init-lsp-bridge")
 (:key "s" :description "Lsp Bridge all symbols" :command lsp-bridge-workspace-list-symbols :filename "init-lsp-bridge")
 (:key "p" :description "Lsp Bridge peek" :command lsp-bridge-peek :filename "init-lsp-bridge")
 (:key "t" :description "Lsp Bridge peek through" :command lsp-bridge-peek-through :filename "init-lsp-bridge"))

(lazy-one-key-create-menu
 "Org"
 (:key "w" :description "Open org file" :command open-my-org-file :filename "init-org")
 (:key "c" :description "Open org capture" :command org-capture :filename "init-org")
 (:key "a" :description "Open org agenda" :command one-key-menu-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org")
 (:key "s" :description "Org search" :command consult-notes)
 (:key "r" :description "Org roam" :command one-key-menu-roam))

(lazy-one-key-create-menu
 "Insert"
 (:key "t" :description "Insert translated name" :command insert-translated-name-insert :filename "init-translated-name")
 (:key "i" :description "Insert import" :command insert-import :filename "init-func")
 (:key "f" :description "Insert file" :command hydra-insert-file/body)
 (:key "j" :description "Insert json to type" :command quicktype))

;; ;;; ### Watch other window ###
;; ;;; --- 滚动其他窗口
(require 'watch-other-window)
(global-set-key (kbd "M-N")
                #'(lambda ()
                    (interactive)
                    (watch-other-window-internal "up"
                                                 (/ (window-body-height) 3))))

(global-set-key (kbd "M-P")
                #'(lambda ()
                    (interactive)
                    (watch-other-window-internal "down"
                                                 (/ (window-body-height) 3))))
;;; symbol overlay
(lazy-load-global-keys
 '(("M-i" . symbol-overlay-put))
 "symbol-overlay")

;;; webjump
;; (lazy-load-global-keys
;;  '(("C-c /" . webjump))
;;  "init-webjump")

(global-set-key (kbd "C-c /") #'google-this)

(provide 'init-key)
