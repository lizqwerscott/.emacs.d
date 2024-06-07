(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-notes
    orderless
    posframe
    request
    websocket
    ag
    rg
    xclip
    helpful
    which-key
    hydra
    pretty-hydra))

(defvar *package-tool-install-list*
  '(try
    centered-cursor-mode
    sudo-edit
    google-this
    interaction-log
    restclient
    dired-git-info
    dired-rsync
    dired-toggle-sudo
    diredfl
    dired-subtree
    elisp-demos
    vterm
    multi-vterm
    (lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert")
    (psearch
     :fetcher github
     :repo "twlz0ne/psearch.el"
     :files ("psearch.el"))
    (advance-words-count
     :fetcher github
     :repo "LdBeth/advance-words-count.el")
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
    csv-mode))

(defvar *package-edit-install-list*
  '(meow
    grugru
    auto-rename-tag
    hungry-delete
    separedit
    symbol-overlay
    aggressive-indent
    apheleia
    avy
    (fingertip :fetcher github :repo "manateelazycat/fingertip")
    (awesome-pair :fetcher github :repo "manateelazycat/awesome-pair")
    (meow-vterm :fetcher github :repo "accelbread/meow-vterm")))

(defvar *package-program-install-list*
  '(dumb-jump
    yasnippet
    macrostep
    eglot
    magit
    magit-delta
    git-cliff
    devdocs
    wucuo
    projection
    eshell-prompt-extras
    fish-completion
    dape
    citre
    (quicktype :fetcher github :repo "artawower/quicktype.el")
    (color-rg :fetcher github
              :repo "manateelazycat/color-rg")
    (peek :fetcher sourcehut :repo "meow_king/peek")

    (auto-save :fetcher github :repo "manateelazycat/auto-save")
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
    lisp-extra-font-lock
    highlight-function-calls
    focus
    goggles
    diff-hl
    rainbow-delimiters
    rainbow-mode
    visual-fill-column
    olivetti
    redacted
    hl-todo
    imenu-list
    outshine
    (indent-bars :fetcher github :repo "jdtsmith/indent-bars")
    (sort-tab :fetcher github
              :repo "manateelazycat/sort-tab")
    (awesome-tray :fetcher github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :fetcher github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag")
    ))

(defvar *package-window-install-list*
  '(shackle
    popper
    ace-window
    (watch-other-window :fetcher github :repo "manateelazycat/watch-other-window")
    ))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :fetcher github
     :repo "manateelazycat/sdcv")
    fanyi
    go-translate
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :fetcher github
     :repo "redguardtoo/pyim-tsinghua-dict"
     :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim"))
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
    pangu-spacing
    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    ))

(defvar *package-ai-install-list*
  '((copilot :fetcher github
             :repo "zerolfx/copilot.el"
             :branch "main"
             :files ("dist" "*.el"))
    gptel))

(defvar *package-rust-install-list*
  '(rust-mode
    cargo))

(defvar *package-common-lisp-install-list*
  '(common-lisp-snippets
    sly
    sly-quicklisp
    sly-asdf
    ))

(defvar *package-scheme-install-list*
  '(geiser
    geiser-guile))

(defvar *package-web-install-list*
  '(web-mode
    pnpm-mode
    ))

(defvar *package-python-install-list*
  '(conda
    pyvenv))

(defvar *package-unity-install-list*
  '((unity :fetcher github :repo "elizagamedev/unity.el")))

(defvar *package-sql-install-list*
  '(sql-indent))

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    ;; tabspaces
    docker
    (screenshot :fetcher github :repo "tecosaur/screenshot")
    (telega :fetcher github
            :repo "zevlg/telega.el"
            :branch "master"
            :files (:defaults "contrib" "etc" "server" "Makefile"))))

(packages! *package-base-install-list*)
(packages! *package-tool-install-list*)
(packages! *package-language-mode-install-list*)
(packages! *package-edit-install-list*)
(packages! *package-program-install-list*)
(packages! *package-ui-install-list*)
(packages! *package-window-install-list*)
(packages! *package-language-install-list*)
(packages! *package-org-install-list*)
(packages! *package-ai-install-list*)
(packages! *package-rust-install-list*)
(packages! *package-common-lisp-install-list*)
(packages! *package-web-install-list*)
(packages! *package-python-install-list*)
(packages! *package-unity-install-list*)
(packages! *package-sql-install-list*)
(packages! *package-another-install-list*)

(provide 'init-packages)
