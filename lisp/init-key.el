(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(global-set-key (kbd "s-x") #'execute-extended-command)

(global-set-key (kbd "s-o") #'other-window)

(use-package one-key
  :quelpa (one-key :fetcher github :repo "manateelazycat/one-key"))

(one-key-create-menu
 "Search"
 '((("l"  . "Search in buffer") . consult-line)
   (("i"  . "Search imenu") . consult-imenu)
   (("m" . "Search imenu int multi buffer") . consult-imenu-multi)
   (("g" . "goto line") . consult-goto-line)
   (("o" . "Search outline") . consult-outline)
   (("s" . "Blink Search") . blink-search)
   (("B" . "Bookmark") . consult-bookmark)))

(one-key-create-menu
 "Buffer"
 '((("b" . "Switch buffer") . consult-buffer)
   (("k" . "Kill buffer") . kill-buffer)
   (("T" . "Switch telega buffers") . telega-switch-buffer)
   (("t" . "Switch telega chat") . telega-chat-with)
   (("r" . "Revert buffer") . revert-buffer)))

(one-key-create-menu
 "File"
 '((("f" . "Find file") . find-file)
   (("r" . "Recent file") . consult-recent-file)
   (("d" . "Find main directory") . open-main-directory)))

(one-key-create-menu
 "EAF"
 '((("o" . "EAF Open anything") . eaf-open)
   (("b" . "EAF Open browser") . eaf-open-browser)
   (("h" . "EAF Open browser with history") . eaf-open-browser-with-history)
   (("s" . "EAF Search") . eaf-search-it)))

(one-key-create-menu
 "sort-tab"
 '((("n" . "Sort tab select next tab") . sort-tab-select-next-tab)
   (("p" . "Sort tab select prev tab") . sort-tab-select-prev-tab)
   (("c" . "Sort tab close current tab") . sort-tab-close-current-tab)
   (("m" . "Sort tab close all mode tabs") . sort-tab-close-mode-tabs)))

(one-key-create-menu
 "Diagnostic"
 '((("n" . "Next diagnostic") . lsp-bridge-diagnostic-jump-next)
   (("p" . "Previous diagnostic") . lsp-bridge-diagnostic-jump-prev)
   (("l" . "Show all diagnostic") . lsp-bridge-diagnostic-list)))

(one-key-create-menu
 "Code"
 '((("r" . "Lsp Bridge rename") . lsp-bridge-rename)
   (("s" . "Lsp Bridge code action") . lsp-bridge-code-action)
   (("a" . "Lsp Bridge Diagnostic") . one-key-menu-diagnostic)))

(one-key-create-menu
 "Useful"
 '((("t" . "Go translate") . gts-do-translate)
   (("g" . "Google this") . google-this)
   (("p" . "Popweb dict bing") . popweb-dict-bing-pointer)))

(one-key-create-menu
 "Org"
 '((("w" . "Open org file") . open-my-org-file)
   (("c" . "Open org capture") . org-capture)
   (("a" . "Open org agenda") . org-agenda)
   (("l" . "Org store link") . org-store-link)))

(provide 'init-key)
