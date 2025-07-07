;; elpaca packages configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(defun packages! (packages)
  (dolist (package packages)
    (elpaca--expand-declaration package 'nil)))

(defvar *package-early-install-list*
  '(no-littering
    exec-path-from-shell

    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")
    (one-key :fetcher github :repo "manateelazycat/one-key")))

(defvar *package-build-in-install-list*
  '(transient))

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
     :fetcher github
     :files (:defaults "bin"))
    posframe
    request
    websocket
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
    dired-collapse
    trashed
    dirvish
    elisp-demos
    (lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert")
    heap
    (p-search :repo "zkry/p-search" :fetcher github)
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
    qml-mode
    sxhkdrc-mode))

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
  `(dumb-jump
    yasnippet
    consult-yasnippet
    macrostep
    ,@(pcase user/lsp-client
        ('eglot
         '(eglot
           (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
           consult-eglot
           flycheck
           consult-flycheck
           flycheck-eglot))
        ('lsp-bridge
         `((lsp-bridge :type git :fetcher github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
           ,@(when (not (display-graphic-p))
               '((popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
                 (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git"))))))
    corfu
    cape
    nerd-icons-corfu
    eldoc-box
    (flyover :fetcher github :repo "konrad1977/flyover")
    magit
    magit-delta
    forge
    difftastic
    devdocs
    wucuo
    compile-multi
    projection
    projection-multi
    eshell-prompt-extras
    esh-help
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
  '((koishi-theme :fetcher github :repo "gynamics/koishi-theme.el")
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
  '(popper
    ace-window
    (watch-other-window :fetcher github :repo "manateelazycat/watch-other-window")
    ))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :fetcher github
     :repo "manateelazycat/sdcv")
    fanyi
    (pdd :fetcher github :repo "lorniu/pdd.el")
    (go-translate :fetcher github :repo "lorniu/go-translate")
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :fetcher github
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
    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    pdf-tools
    (org-count-words :fetcher github :repo "Elilif/org-count-words")))

(defvar *package-ai-install-list*
  (append '((gptel :fetcher github
                   :repo "karthink/gptel")
            (gptel-aibo :fetcher github
                        :repo "dolmens/gptel-aibo")
            (mcp :fetcher github
                 :repo "lizqwerscott/mcp.el"))
          (pcase user/ai-completion
            ('copilot
             '((copilot :fetcher github
                        :repo "zerolfx/copilot.el"
                        :branch "main")))
            ('minuet
             '((minuet :fetcher github
                       :repo "milanglacier/minuet-ai.el"))))
          (when user/aider
            '((aidermacs :fetcher github
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
            :branch "master")
    (consult-omni :type git :fetcher github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))))

(packages! *package-early-install-list*)

(setq vterm-always-compile-module t)
(packages!
 '(eat
   vterm
   (meow-vterm :fetcher github :repo "accelbread/meow-vterm")
   (multi-vterm :fetcher github :repo "lizqwerscott/multi-vterm")))

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

(elpaca-process-queues)

(elpaca-wait)

(provide 'init-packages)
