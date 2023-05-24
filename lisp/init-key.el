(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(global-set-key (kbd "s-x") #'execute-extended-command)

(global-set-key (kbd "s-o") #'other-window)

(use-package one-key
  :quelpa (one-key :fetcher github :repo "manateelazycat/one-key"))

;;;###autoload
(defmacro lazy-one-key-create-menu (title &rest keybinds)
  (let (one-key-key-alist)
    (dolist (ele keybinds)
      (when (plist-get ele :filename)
        (autoload (plist-get ele :command) (plist-get ele :filename) nil t))
      (push
       (cons (cons (plist-get ele :key) (plist-get ele :description)) (plist-get ele :command))
       one-key-key-alist))
    `(one-key-create-menu ,title (quote ,one-key-key-alist))))

(lazy-one-key-create-menu
 "Toggle"
 (:key "t" :description "Toggle telega" :command +lizqwer/toggle-telega :filename "init-telega")
 (:key "c" :description "Toggle copilot" :command +lizqwer/toggle-copilot :filename "init-copilot")
 (:key "v" :description "Toggle vterm" :command vterm-toggle :filename "init-vterm")
 (:key "b" :description "Toggle big screen mode" :command open-big-screen-mode :filename "init-big-screen")
 (:key "T" :description "Toggle transparent" :command +lizqwer/toggle-transparent :filename "init-func")
 (:key "p" :description "Toggle proxy" :command +lizqwer/toggle-proxy :filename "init-func"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in buffer" :command consult-line)
 (:key "i" :description "Search imenu" :command consult-imenu)
 (:key "m" :description "Search imenu int multi buffer" :command consult-imenu-multi)
 (:key "g" :description "Goto line" :command consult-goto-line)
 (:key "o" :description "Search outline" :command consult-outline)
 (:key "B" :description "Bookmark" :command consult-bookmark)
 (:key "s" :description "Blink Search" :command blink-search :filename "init-blink-search"))

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
   (("d" . "Find main directory") . open-main-directory)
   (("j" . "Find in main dir") . (lambda ()
                                   (interactive)
                                   (ido-find-file-in-dir "~/")))))

(lazy-one-key-create-menu
 "EAF"
 (:key "o" :description "EAF Open anything" :command eaf-open  :filename "init-eaf")
 (:key "b" :description "EAF Open browser" :command eaf-open-browser  :filename "init-eaf")
 (:key "h" :description "EAF Open browser with history" :command eaf-open-browser-with-history  :filename "init-eaf")
 (:key "s" :description "EAF Search" :command eaf-search-it  :filename "init-eaf")
 )

(one-key-create-menu
 "sort-tab"
 '((("n" . "Sort tab select next tab") . sort-tab-select-next-tab)
   (("p" . "Sort tab select prev tab") . sort-tab-select-prev-tab)
   (("c" . "Sort tab close current tab") . sort-tab-close-current-tab)
   (("m" . "Sort tab close all mode tabs") . sort-tab-close-mode-tabs)))

(lazy-one-key-create-menu
 "Useful"
 (:key "t" :description "Go translate" :command gts-do-translate :filename "init-go-translate")
 (:key "g" :description "Google this" :command google-this :filename "init-google-this")
 (:key "p" :description "Popweb dict bing" :command popweb-dict-bing-pointer :filename "init-popweb")
 (:key "S" :description "Sudo edit" :command sudo-edit :filename "init-sudo-edit"))

(lazy-one-key-create-menu
 "Org"
 (:key "w" :description "Open org file" :command open-my-org-file :filename "init-org")
 (:key "c" :description "Open org capture" :command org-capture :filename "init-org")
 (:key "a" :description "Open org agenda" :command org-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org"))

(provide 'init-key)
