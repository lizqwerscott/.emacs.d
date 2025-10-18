;;; init-packages.el --- elpaca packages configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(with-eval-after-load 'elpaca-ui
  (keymap-binds elpaca-ui-mode-map
    ("p" . previous-line)
    ("F" . elpaca-ui-mark-pull)))

(defun packages! (packages)
  "Install PACKAGES."
  (dolist (package packages)
    (elpaca--expand-declaration package 'nil)))

(defun wait-packages! (packages)
  "Wait install PACKAGES."
  (packages! packages)
  (elpaca-process-queues)

  (elpaca-wait))

(defun +elpaca-with-github-straight-mirror (recipe)
  "Replace github with emacs-straight in package RECIPE."
  (when-let* ((source (plist-get recipe :source))
              ((string-match-p "GNU ELPA" source))
              (repo (plist-get recipe :repo))
              (url (or (car-safe repo) repo))
              ((string-match-p "github.com" url))
              ((not (string-match-p "emacsmirror/gnu_elpa" url)))
              ((not (string-match-p "emacs-mirror" url)))
              (mirror (replace-regexp-in-string "https://github.com/\\([^/]+\\)/"
                                                (concat "https://github.com/" "emacs-straight" "/")
                                                url)))
    (list :repo (if (consp repo) (cons mirror (cdr repo)) mirror))))

(add-to-list 'elpaca-recipe-functions #'+elpaca-with-github-straight-mirror)

(defvar *package-early-install-list*
  '(no-littering
    exec-path-from-shell

    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")))

(defvar *package-build-in-install-list*
  '(transient))

(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-dir
    bufferfile
    bufferlo
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
    eros
    heap
    (p-search :repo "zkry/p-search" :fetcher github)
    gif-screencast
    keycast
    gnuplot
    cal-china-x
    consult-gh
    consult-gh-forge
    backup-walker))

(defvar *package-language-mode-install-list*
  '(markdown-mode
    log4j-mode
    just-mode
    yaml-mode
    elvish-mode
    git-modes
    csv-mode
    qml-mode
    sxhkdrc-mode
    meson-mode))

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
    package-lint
    ,@(pcase user/lsp-client
        ('eglot
         `(eglot
           (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
           consult-eglot
           flycheck
           consult-flycheck
           flycheck-eglot
           flycheck-package
           ,@(if user/flyoverp
                 '((flyover :fetcher github :repo "konrad1977/flyover"))
               '(flycheck-posframe))))
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
    (cond-let :fetcher github :repo "tarsius/cond-let")
    magit
    magit-delta
    git-link
    forge
    difftastic
    devdocs
    jinx
    compile-multi
    projection
    projection-multi
    eshell-prompt-extras
    esh-help
    dape
    citre
    (xmake :fetcher github :repo "lizqwerscott/xmake-emacs")
    (quicktype :fetcher github :repo "artawower/quicktype.el")
    super-save
    (rsync-project-mode :fetcher github :repo "lizqwerscott/rsync-project-mode")))

(defvar *package-ui-install-list*
  '((koishi-theme :fetcher github :repo "gynamics/koishi-theme.el")
    doom-themes
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    page-break-lines
    ligature
    dashboard
    (grid :fetcher github :repo "ichernyshovvv/grid.el")
    enlight
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
    consult-todo
    imenu-list
    outli
    (indent-bars :fetcher github :repo "jdtsmith/indent-bars")
    (awesome-tray :fetcher github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :fetcher github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag")
    nerd-icons-ibuffer
    casual
    inhibit-mouse))

(defvar *package-window-install-list*
  '(popper
    ace-window))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :fetcher github
     :repo "manateelazycat/sdcv")
    fanyi
    (pdd :fetcher github :repo "lorniu/pdd.el")
    (gt :fetcher github :repo "lorniu/gt.el")
    unicode-math-input
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :fetcher github
     :repo "redguardtoo/pyim-tsinghua-dict")))

(defvar *package-org-install-list*
  '(consult-notes
    org-superstar
    org-fancy-priorities
    ox-reveal
    ox-hugo
    org-appear
    org-fragtog
    valign
    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    (org-count-words :fetcher github :repo "Elilif/org-count-words")
    org-rich-yank
    htmlize))

(defvar *package-write-install-list*
  '(nov
    pdf-tools
    ;; (reader :fetcher codeberg :repo "divyaranjan/emacs-reader"
    ;;       	:files ("*.el" "render-core.so")
    ;;       	:pre-build ("make" "all"))
    biblio
    citar
    (oxr :fetcher github :repo "bdarcus/oxr")
    titlecase
    denote
    denote-menu
    consult-denote
    denote-journal
    denote-org
    denote-sequence
    citar-embark
    citar-denote
    denote-explore
    ox-epub
    (auctex :fetcher github :repo "emacs-straight/auctex" :branch "master")
    cdlatex))

(defvar *package-ai-install-list*
  (append '((gptel :fetcher github
                   :repo "karthink/gptel")
            (gptel-aibo :fetcher github
                        :repo "dolmens/gptel-aibo")
            (mcp :fetcher github
                 :repo "lizqwerscott/mcp.el")
            (macher :host github :repo "kmontag/macher"))
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

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    docker
    (screenshot :fetcher github :repo "tecosaur/screenshot")
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
         *package-write-install-list*

         *package-ai-install-list*

         *package-another-install-list*))

(elpaca-process-queues)

(elpaca-wait)

(provide 'init-packages)
;;; init-packages.el ends here
