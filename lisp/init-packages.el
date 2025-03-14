(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-notes
    consult-yasnippet
    orderless
    embark
    embark-consult
    fussy
    (flx-rs
     :repo "jcs-elpa/flx-rs"
     :fetcher github)
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
    dired-rsync-transient
    dired-toggle-sudo
    diredfl
    dired-subtree
    dired-quick-sort
    dirvish
    elisp-demos
    (lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert")
    (psearch
     :fetcher github
     :repo "twlz0ne/psearch.el")
    heap
    (p-search :repo "zkry/p-search" :fetcher github)
    gif-screencast
    keycast
    gnuplot
    cal-china-x))

(defvar *package-language-mode-install-list*
  '(markdown-mode
    log4j-mode
    just-mode
    yaml-mode
    go-mode
    haskell-mode
    elvish-mode
    git-modes
    csv-mode
    qml-mode))

(defvar *package-edit-install-list*
  '((meow :fetcher github :repo "meow-edit/meow")
    meow-tree-sitter
    (repeat-fu :fetcher codeberg :repo "ideasman42/emacs-repeat-fu")
    grugru
    auto-rename-tag
    hungry-delete
    separedit
    symbol-overlay
    aggressive-indent
    apheleia
    vundo
    outline-indent
    visual-replace
    (fingertip :fetcher github :repo "manateelazycat/fingertip")))

(defvar *package-program-install-list*
  '(dumb-jump
    yasnippet
    macrostep
    eglot
    (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
    consult-eglot
    corfu
    cape
    nerd-icons-corfu
    eldoc-box
    flymake-popon
    magit
    magit-delta
    forge
    difftastic
    devdocs
    wucuo
    projection
    disproject
    eshell-prompt-extras
    fish-completion
    dape
    citre
    (xmake :fetcher github :repo "lizqwerscott/xmake-emacs")
    (quicktype :fetcher github :repo "artawower/quicktype.el")
    (color-rg :fetcher github
              :repo "manateelazycat/color-rg")
    (peek :fetcher sourcehut :repo "meow_king/peek")

    (auto-save :fetcher github :repo "manateelazycat/auto-save")
    super-save
    (rsync-project-mode :fetcher github :repo "lizqwerscott/rsync-project-mode")
    (image-slicing :fetcher github :repo "ginqi7/image-slicing")))

(defvar *package-ui-install-list*
  '(nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    page-break-lines
    ligature
    dashboard
    doom-modeline
    pretty-mode
    lisp-extra-font-lock
    highlight-function-calls
    focus
    pulsar
    diff-hl
    rainbow-delimiters
    highlight-parentheses
    colorful-mode
    visual-fill-column
    olivetti
    redacted
    hl-todo
    imenu-list
    outshine
    (indent-bars :fetcher github :repo "jdtsmith/indent-bars")
    (awesome-tray :fetcher github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :fetcher github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag")
    buffer-name-relative
    nerd-icons-ibuffer
    casual))

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
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :fetcher github
     :repo "redguardtoo/pyim-tsinghua-dict")))

(defvar *package-org-install-list*
  '(org-bullets
    org-fancy-priorities
    org-roam
    org-roam-ui
    ox-reveal
    ox-hugo
    org-appear
    valign
    pangu-spacing
    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    pdf-tools))

(defvar *package-ai-install-list*
  (append '((gptel :fetcher github
                   :repo "karthink/gptel")
            (gptel-quick :fetcher github
                         :repo "karthink/gptel-quick")
            (gptel-aibo :fetcher github
                        :repo "dolmens/gptel-aibo"))
          (pcase user/ai-completion
            ('codeium
             '((codeium :fetcher github
                        :repo "Exafunction/codeium.el")
               (codeium-overlay :fetcher github
                                :repo "tjohnman/codeium-overlay.el")))
            ('copilot
             '((copilot :fetcher github
                        :repo "zerolfx/copilot.el"
                        :branch "main")))
            ('minuet
             '((minuet :fetcher github
                       :repo "milanglacier/minuet-ai.el"))))
          (when user/aider
            '((aider :fetcher github
                     :repo "tninja/aider.el")))))

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

(defvar *package-godot-install-list*
  '(gdscript-mode))

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    ;; tabspaces
    docker
    (screenshot :fetcher github :repo "tecosaur/screenshot")
    (telega-url-shorten-nerd :fetcher github
                             :repo "lizqwerscott/telega-url-shorten-nerd")
    (telega :fetcher github
            :repo "zevlg/telega.el"
            :branch "master")))

(packages!
 '(vterm
   (meow-vterm :fetcher github :repo "accelbread/meow-vterm")))

(packages!
 (append *package-base-install-list*
         *package-tool-install-list*
         *package-language-mode-install-list*
         *package-edit-install-list*
         *package-program-install-list*
         *package-ui-install-list*
         *package-window-install-list*
         *package-language-install-list*
         *package-org-install-list*
         *package-ai-install-list*
         *package-rust-install-list*
         *package-common-lisp-install-list*
         *package-web-install-list*
         *package-python-install-list*
         (when user/unity
           *package-unity-install-list*)
         *package-sql-install-list*
         (when user/godot
           *package-godot-install-list*)
         *package-another-install-list*))

(provide 'init-packages)
