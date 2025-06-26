(defvar *package-build-in-install-list*
  '((org :type built-in)
    (transient :type built-in)))

(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    orderless
    embark
    embark-consult
    fussy
    (flx-rs
     :repo "jcs-elpa/flx-rs"
     :host github
     :files (:defaults "bin"))
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
  '(centered-cursor-mode
    sudo-edit
    interaction-log
    restclient
    dired-git-info
    dired-rsync
    dired-rsync-transient
    dired-toggle-sudo
    diredfl
    dired-subtree
    dired-quick-sort
    trashed
    dirvish
    elisp-demos
    (lazy-revert :host github :repo "yilin-zhang/lazy-revert")
    (psearch
     :host github
     :repo "twlz0ne/psearch.el")
    heap
    (p-search :repo "zkry/p-search" :host github)
    gif-screencast
    keycast
    gnuplot
    cal-china-x
    consult-gh
    consult-gh-forge))

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
  '((meow :host github :repo "meow-edit/meow")
    meow-tree-sitter
    (repeat-fu :host codeberg :repo "ideasman42/emacs-repeat-fu")
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
    (fingertip :host github :repo "manateelazycat/fingertip")))

(defvar *package-program-install-list*
  `(dumb-jump
    yasnippet
    consult-yasnippet
    macrostep
    ,@(pcase user/lsp-client
        ('eglot
         '(eglot
           (eglot-booster :host github :repo "jdtsmith/eglot-booster")
           consult-eglot))
        ('lsp-bridge
         `((lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
           ,@(when (not (display-graphic-p))
               '((popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
                 (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git"))))))
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
    eshell-prompt-extras
    esh-help
    fish-completion
    dape
    citre
    (xmake :host github :repo "lizqwerscott/xmake-emacs")
    (quicktype :host github :repo "artawower/quicktype.el")
    (color-rg :host github
              :repo "manateelazycat/color-rg")
    (peek :host sourcehut :repo "meow_king/peek")

    (auto-save :host github :repo "manateelazycat/auto-save")
    super-save
    (rsync-project-mode :host github :repo "lizqwerscott/rsync-project-mode")
    (image-slicing :host github :repo "ginqi7/image-slicing")))

(defvar *package-ui-install-list*
  '((koishi-theme :host github :repo "gynamics/koishi-theme.el")
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    page-break-lines
    ligature
    dashboard
    doom-modeline
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
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (awesome-tray :host github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :host github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :host github :repo "manateelazycat/highlight-matching-tag")
    buffer-name-relative
    nerd-icons-ibuffer
    casual))

(defvar *package-window-install-list*
  '(popper
    ace-window
    (watch-other-window :host github :repo "manateelazycat/watch-other-window")
    ))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :host github
     :repo "manateelazycat/sdcv")
    fanyi
    go-translate
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :host github
     :repo "redguardtoo/pyim-tsinghua-dict")))

(defvar *package-org-install-list*
  '(consult-notes
    org-bullets
    org-fancy-priorities
    org-roam
    org-roam-ui
    ox-reveal
    ox-hugo
    org-appear
    valign
    pangu-spacing
    (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
    pdf-tools
    (org-count-words :host github :repo "Elilif/org-count-words")))

(defvar *package-ai-install-list*
  (append '((gptel :host github
                   :repo "karthink/gptel")
            (gptel-quick :host github
                         :repo "karthink/gptel-quick")
            (gptel-aibo :host github
                        :repo "dolmens/gptel-aibo")
            (mcp :host github
                 :repo "lizqwerscott/mcp.el"))
          (pcase user/ai-completion
            ('copilot
             '((copilot :host github
                        :repo "zerolfx/copilot.el"
                        :branch "main")))
            ('minuet
             '((minuet :host github
                       :repo "milanglacier/minuet-ai.el"))))
          (when user/aider
            '((aidermacs :host github
                         :repo "MatthewZMD/aidermacs")))))

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

(defvar *package-zig-install-list*
  '(zig-mode
    zig-ts-mode))

(defvar *package-unity-install-list*
  '((unity :host github :repo "elizagamedev/unity.el")))

(defvar *package-sql-install-list*
  '(sql-indent))

(defvar *package-godot-install-list*
  '(gdscript-mode))

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    ;; tabspaces
    docker
    (screenshot :host github :repo "tecosaur/screenshot")
    (telega-url-shorten-nerd :host github
                             :repo "lizqwerscott/telega-url-shorten-nerd")
    (telega :host github
            :repo "zevlg/telega.el"
            :branch "master")
    (consult-omni :type git :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))))

(setq vterm-always-compile-module t)
(packages!
 '(eat
   vterm
   (meow-vterm :host github :repo "accelbread/meow-vterm")
   (multi-vterm :host github :repo "lizqwerscott/multi-vterm")))

(packages!
 (append *package-build-in-install-list*
         *package-base-install-list*
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
         *package-zig-install-list*
         (when user/unity
           *package-unity-install-list*)
         *package-sql-install-list*
         (when user/godot
           *package-godot-install-list*)
         *package-another-install-list*))

(provide 'init-packages)
