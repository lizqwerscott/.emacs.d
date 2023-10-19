(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(global-set-key (kbd "s-x") #'execute-extended-command)

(global-set-key (kbd "s-o") #'other-window)

(require 'one-key)

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
 (:key "c" :description "Toggle copilot" :command +lizqwer/toggle-copilot :filename "init-copilot")
 (:key "v" :description "Toggle vterm" :command vterm-toggle :filename "init-vterm")
 (:key "b" :description "Toggle big screen mode" :command open-big-screen-mode :filename "init-big-screen")
 (:key "T" :description "Toggle transparent" :command +lizqwer/toggle-transparent :filename "init-func")
 (:key "p" :description "Toggle proxy" :command +lizqwer/toggle-proxy :filename "init-func")
 (:key "r" :description "Toggle redacted" :command redacted-mode)
 (:key "l" :description "Lock screen" :command +lizqwer/toggle-lock :filename "init-func"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in buffer" :command consult-line)
 (:key "i" :description "Search imenu" :command consult-imenu)
 (:key "m" :description "Search imenu int multi buffer" :command consult-imenu-multi)
 (:key "g" :description "Goto line" :command consult-goto-line)
 (:key "o" :description "Search outline" :command consult-outline)
 (:key "B" :description "Bookmark" :command consult-bookmark)
 (:key "s" :description "Blink Search" :command blink-search :filename "init-blink-search")
 (:key "j" :description "color rg search symbol in project" :command color-rg-search-input-in-project :filename "color-rg"))

(lazy-one-key-create-menu
 "Buffer"
 (:key "b" :description "Switch buffer" :command consult-buffer)
 (:key "k" :description "Kill buffer" :commit kill-buffer-and-window)
 (:key "T" :description "Switch telega buffers" :command telega-switch-buffer :filename "init-telega")
 (:key "t" :description "Switch telega chat" :command telega-chat-with :filename "init-telega")
 (:key "r" :description "Revert buffer" :command revert-buffer))

(one-key-create-menu
 "File"
 '((("f" . "Find file") . find-file)
   (("r" . "Recent file") . consult-recent-file)
   (("j" . "Find in main dir") . (lambda ()
                                   (interactive)
                                   (ido-find-file-in-dir "~/")))
   (("p" . "Find in project") . (lambda ()
                                  (interactive)
                                  (require 'project)
                                  (ido-find-file-in-dir (project-root (project-current)))))))

(lazy-one-key-create-menu
 "Directory"
 (:key "h" :description "Home Dir" :run (lambda () (interactive) (eaf-open-in-file-manager "~/")) :command eaf-open-in-file-manager :filename "init-eaf")
 (:key "c" :description "Config Dir" :run (lambda () (interactive) (eaf-open-in-file-manager "~/.emacs.d/")) :command eaf-open-in-file-manager :filename "init-eaf")
 (:key "g" :description "My Github Dir" :run (lambda () (interactive) (eaf-open-in-file-manager "~/github/")) :command eaf-open-in-file-manager :filename "init-eaf")
 (:key "p" :description "My Project Dir" :run (lambda () (interactive) (eaf-open-in-file-manager "~/MyProject/")) :command eaf-open-in-file-manager :filename "init-eaf")
 (:key "d" :description "My Document Dir" :run (lambda () (interactive) (eaf-open-in-file-manager "~/Documents/")) :command eaf-open-in-file-manager :filename "init-eaf"))

(lazy-one-key-create-menu
 "EAF"
 (:key "o" :description "EAF Open anything" :command eaf-open  :filename "init-eaf")
 (:key "b" :description "EAF Open browser" :command eaf-open-browser  :filename "init-eaf")
 (:key "h" :description "EAF Open browser with history" :command eaf-open-browser-with-history  :filename "init-eaf")
 (:key "s" :description "EAF Search" :command eaf-search-it  :filename "init-eaf")
 (:key "m" :description "EAF Music" :command eaf-open-cloud-music :filename "init-eaf")
 )

(one-key-create-menu
 "sort-tab"
 '((("n" . "Sort tab select next tab") . sort-tab-select-next-tab)
   (("p" . "Sort tab select prev tab") . sort-tab-select-prev-tab)
   (("c" . "Sort tab close current tab") . sort-tab-close-current-tab)
   (("m" . "Sort tab close all mode tabs") . sort-tab-close-mode-tabs)))

(one-key-create-menu
 "Workspace"
 '((("l" . "Workspace switch or create workspace") . tabspaces-switch-or-create-workspace)
   (("p" . "Workspace project switch or create") . tabspaces-open-or-create-project-and-workspace)
   (("f" . "Workspace projecr"))
   (("k" . "Workspace kill workspace") . tabspaces-kill-buffers-close-workspace)))

(lazy-one-key-create-menu
 "Useful"
 (:key "t" :description "Go translate" :command gts-do-translate :filename "init-go-translate")
 (:key "g" :description "Google this" :command google-this :filename "init-google-this")
 (:key "p" :description "Popweb dict bing" :command popweb-dict-bing-pointer :filename "init-popweb")
 (:key "S" :description "Sudo edit" :command sudo-edit :filename "sudo-edit")
 (:key "c" :description "Copy File name" :command +lizqwer/copy-file-name-to-clipboard :filename "init-func")
 (:key "C" :description "Copy File Path" :command +lizqwer/copy-file-path-to-clipboard :filename "init-func")
 (:key "e" :description "Toggle sdcv" :command lsp-bridge-toggle-sdcv-helper :filename "init-lsp-bridge")
 (:key "D" :description "Docker" :command docker)
 (:key "p" :description "peek code" :command peek-overlay-dwim :filename "init-peek")
 (:key "d" :description "Devdocs" :command devdocs-lookup))

(lazy-one-key-create-menu
 "Code"
 (:key "h" :description "Show document" :command lsp-bridge-popup-documentation :filename "init-lsp-bridge")
 (:key "j" :description "Scroll doc up" :command lsp-bridge-popup-documentation-scroll-up :filename "init-lsp-bridge")
 (:key "k" :description "Scroll doc down" :command lsp-bridge-popup-documentation-scroll-down :filename "init-lsp-bridge")
 (:key "f" :description "Format code" :command apheleia-format-buffer)
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
 (:key "a" :description "Open org agenda" :command org-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org"))

;;; ### Watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-J" . watch-other-window-up)        ;向下滚动其他窗口
   ("M-K" . watch-other-window-down)      ;向上滚动其他窗口
   ("M-<" . watch-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . watch-other-window-down-line) ;向上滚动其他窗口一行
   )
 "watch-other-window")

(provide 'init-key)
