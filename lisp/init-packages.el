(defvar *package-need-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    orderless
    posframe
    try
    symbol-overlay
    color-identifiers-mode
    focus
    centered-cursor-mode
    goggles
    diff-hl
    rainbow-delimiters
    rainbow-mode
    shackle
    separedit
    request
    websocket
    interaction-log
    visual-fill-column
    helpful
    dumb-jump
    yasnippet
    common-lisp-snippets
    markdown-mode
    rust-mode
    cargo
    macrostep
    google-this
    sly
    sly-quicklisp
    sly-asdf
    aggressive-indent
    which-key
    apheleia
    eglot
    xclip
    pyim
    pyim-basedict
    olivetti
    ag
    rg
    magit
    magit-todos
    pretty-mode
    diredfl
    dired-subtree
    solarized-theme
    nerd-icons
    nerd-icons-completion
    nerd-icons-dired
    imenu-list
    hl-todo
    avy
    ace-window
    sudo-edit
    devdocs
    elisp-demos
    go-mode
    haskell-mode
    jsonian
    web-mode
    restclient
    code-stats
    outshine
    grugru
    pnpm-mode
    tabspaces
    wucuo
    docker
    elvish-mode
    redacted
    projection
    valign
    org-bullets
    org-fancy-priorities
    eshell-prompt-extras
    fish-completion
    ox-hugo
    meow
    org-download
    immersive-translate
    vterm
    multi-vterm))

(package-check-install *package-need-install-list*)


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

(provide 'init-packages)
