(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-notes
    embark
    embark-consult
    orderless
    posframe
    vundo
    request
    websocket
    ag
    rg
    xclip
    helpful
    which-key))

(defvar *package-need-install-list*
  '(try
    symbol-overlay
    centered-cursor-mode
    separedit
    interaction-log
    google-this
    aggressive-indent
    apheleia
    pyim
    pyim-basedict
    magit
    magit-delta
    magit-todos
    dired-rsync
    dired-toggle-sudo
    diredfl
    dired-subtree
    imenu-list
    avy
    sudo-edit
    devdocs
    elisp-demos
    restclient
    code-stats
    outshine
    ;; tabspaces
    wucuo
    docker
    projection
    pangu-spacing
    eshell-prompt-extras
    fish-completion
    immersive-translate
    elfeed
    vterm
    multi-vterm
    ))

(defvar *package-language-mode-install-list*
  '(markdown-mode
    log4j-mode
    just-mode
    yaml-mode
    go-mode
    haskell-mode
    jsonian
    elvish-mode
    git-modes
    csv-mode
    ))

(defvar *package-edit-install-list*
  '(meow
    grugru
    auto-rename-tag
    hungry-delete
    ))

(defvar *package-program-install-list*
  '(dumb-jump
    yasnippet
    macrostep
    eglot
    ))

(defvar *package-ui-install-list*
  '(solarized-theme
    all-the-icons
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    doom-modeline
    pretty-mode
    color-identifiers-mode
    focus
    goggles
    diff-hl
    rainbow-delimiters
    rainbow-mode
    visual-fill-column
    olivetti
    redacted
    hl-todo
    ))

(defvar *package-window-install-list*
  '(shackle
    popper
    ace-window
    ))

(defvar *package-org-install-list*
  '(org-bullets
    org-fancy-priorities
    org-roam
    org-roam-ui
    ox-reveal
    ox-hugo
    org-download
    org-appear
    valign
    ))

(defvar *package-rust-install-list*
  '(rust-mode
    cargo))

(defvar *package-common-lisp-install-list*
  '(common-lisp-snippets
    sly
    sly-quicklisp
    sly-asdf
    ))

(defvar *package-web-install-list*
  '(web-mode
    pnpm-mode
    ))

(package-check-install *package-base-install-list*)
(package-check-install *package-need-install-list*)
(package-check-install *package-language-mode-install-list*)
(package-check-install *package-edit-install-list*)
(package-check-install *package-program-install-list*)
(package-check-install *package-ui-install-list*)
(package-check-install *package-window-install-list*)
(package-check-install *package-org-install-list*)
(package-check-install *package-rust-install-list*)
(package-check-install *package-common-lisp-install-list*)
(package-check-install *package-web-install-list*)

(quelpa '(telega :fetcher github
                 :repo "zevlg/telega.el"
                 :branch "master"
                 :files (:defaults "contrib" "etc" "server" "Makefile")))

(quelpa '(copilot :fetcher github
                  :repo "zerolfx/copilot.el"
                  :branch "main"
                  :files ("dist" "*.el")))

(quelpa '(color-rg :fetcher github
                   :repo "manateelazycat/color-rg"))

(quelpa '(sort-tab :fetcher github
                   :repo "manateelazycat/sort-tab"))

(quelpa '(awesome-tray :fetcher github
                       :repo "manateelazycat/awesome-tray"))

(quelpa '(breadcrumb :fetcher github
                     :repo "joaotavora/breadcrumb"))

(quelpa '(auto-save :fetcher github :repo "manateelazycat/auto-save"))

(quelpa '(lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert"))

(quelpa '(peek :fetcher sourcehut :repo "meow_king/peek"))

(quelpa '(watch-other-window :fetcher github :repo "manateelazycat/watch-other-window"))

(quelpa '(highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag"))

;; (quelpa '(holo-layer :fetcher github :repo "manateelazycat/holo-layer" :files ("*.el" "*.py" "swaymsg-treefetch")))

(quelpa '(psearch
          :fetcher github
          :repo "twlz0ne/psearch.el"
          :files ("psearch.el")))

(quelpa '(advance-words-count
          :fetcher github
          :repo "LdBeth/advance-words-count.el"))
;; 有 bug
;; (quelpa '(vue-ts-mode
;;           :fetcher github
;;           :repo "8uff3r/vue-ts-mode"))

(quelpa '(sdcv
          :fetcher github
          :repo "manateelazycat/sdcv"))

(quelpa '(pyim-tsinghua-dict
          :fetcher github
          :repo "redguardtoo/pyim-tsinghua-dict"
          :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim")))

(quelpa '(awesome-pair :fetcher github :repo "manateelazycat/awesome-pair"))

(quelpa '(fingertip :fetcher github :repo "manateelazycat/fingertip"))

(quelpa '(indent-bars :fetcher github :repo "jdtsmith/indent-bars"))

(quelpa '(screenshot :fetcher github :repo "tecosaur/screenshot"))

(quelpa '(unity :fetcher github :repo "elizagamedev/unity.el"))

(quelpa '(org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent"))

(quelpa '(quicktype :fetcher github :repo "artawower/quicktype.el"))

(provide 'init-packages)
