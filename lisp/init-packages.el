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

;; aot native
(defcustom elpaca-aot-native-compilation t
  "When non-nil, add ahead of time native compilation to `elpaca-build-steps'."
  :type 'boolean)

(setq elpaca-build-steps
      `(
        elpaca--clone elpaca--configure-remotes elpaca--checkout-ref
        elpaca--run-pre-build-commands elpaca--queue-dependencies
        elpaca--check-version elpaca--link-build-files
        elpaca--generate-autoloads-async elpaca--byte-compile
        ,@(and elpaca-aot-native-compilation (fboundp 'native-comp-available-p)
               (native-comp-available-p) '(elpaca--native-compile))
        elpaca--compile-info elpaca--install-info elpaca--add-info-path
        elpaca--run-post-build-commands elpaca--activate-package))

(defun elpaca--native-compile (e)
  "Native compile E's package."
  ;; Assumes all dependencies are 'built
  (let ((default-directory (elpaca<-build-dir e)))
    (elpaca--signal e (concat "Native compiling " default-directory) 'native-compilation)
    (elpaca--make-process e
      :name "native-compile"
      :command `(,(elpaca--emacs-path) "-Q" "-L" "."
                 ,@(cl-loop for dep in (elpaca-dependencies (elpaca<-id e) '(emacs))
                            for item = (elpaca-get dep)
                            for build-dir = (and item (elpaca<-build-dir item))
                            when build-dir append (list "-L" build-dir))
                 ;; Inherit eln load-path in child process. Otherwise, default assumed.
                 "--eval" ,(format "%S" `(setq native-comp-eln-load-path ',native-comp-eln-load-path))
                 "--batch" "-f" "batch-native-compile"
                 ,@(directory-files-recursively default-directory "\\.el$"))
      :sentinel (apply-partially #'elpaca--process-sentinel "Native compilation complete" nil))))

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

(defun get-repo-info-from-url (url)
  "Get repo info from URL.
return (HOSTING-SITE OWNER REPO-NAME)。"
  (let ((patterns
         '((github . "https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (gitlab . "https?://gitlab\\.com/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (bitbucket . "https?://bitbucket\\.org/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (sourcehut . "https?://git\\.sr\\.ht/~\\([^/]+\\)/\\([^/]+\\)")
           (codeberg . "https?://codeberg\\.org/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?"))))
    (catch 'found
      (dolist (pattern patterns)
        (let ((site (car pattern))
              (regexp (cdr pattern)))
          (when (string-match regexp url)
            (throw 'found (list site
                                (match-string 1 url)
                                (match-string 2 url))))))
      nil)))

(defun elpaca-get-recipe-from-clipboard-url ()
  "Elpaca get recipe from clipboard url."
  (interactive)
  (cl-assert (or (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0))
                 (string-match-p "^git@.*:" (current-kill 0)))
             nil "No URL in clipboard")
  (when-let* ((url (current-kill 0))
              (info (get-repo-info-from-url url))
              (fetcher (car info))
              (owner (elt info 1))
              (repo-name (elt info 2))
              (repo-name (replace-regexp-in-string "\\.git$" "" repo-name))
              (recipe (format "(%s :fetcher %s :repo \"%s/%s\")"
                              repo-name
                              fetcher
                              owner
                              repo-name)))
    (insert recipe)))

(defvar *package-early-install-list*
  '(no-littering
    exec-path-from-shell

    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")))

(defvar *package-built-in-install-list*
  '((transient :wait t)
    (org :repo "https://code.tecosaur.net/tec/org-mode.git/"
         :branch "dev"
         :wait t)))

(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-dir
    bufferfile
    file-info
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
    dired-preview
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
    (fingertip :fetcher github :repo "manateelazycat/fingertip")
    puni))

(defvar *package-program-install-list*
  `(yasnippet
    yasnippet-capf
    consult-yasnippet
    macrostep
    package-lint
    ,@(pcase user/lsp-client
        ('eglot
         `((eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
           consult-eglot
           ,@(when user/flyoverp
               '(flycheck
                 (flyover :fetcher github :repo "konrad1977/flyover")))))
        ('lsp-bridge
         `(dumb-jump
           (lsp-bridge :type git :fetcher github :repo "manateelazycat/lsp-bridge"
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
    (blame-reveal :fetcher github :repo "LuciusChen/blame-reveal")
    devdocs
    jinx
    compile-multi
    projection
    projection-multi
    esh-help
    dape
    citre
    (xmake :fetcher github :repo "lizqwerscott/xmake-emacs")
    (quicktype :fetcher github :repo "artawower/quicktype.el")
    super-save
    (rsync-project-mode :fetcher github :repo "lizqwerscott/rsync-project-mode")))

(defvar *package-ui-install-list*
  '(ef-themes
    (koishi-theme :fetcher github :repo "gynamics/koishi-theme.el")
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
    nerd-icons-ibuffer
    casual
    inhibit-mouse
    (knockknock :fetcher github :repo "konrad1977/knockknock")
    (nano-calendar :fetcher github :repo "rougier/nano-calendar")))

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
    (oxr :fetcher github :repo "liyingzhi/oxr" :branch "fix-insert-name")
    titlecase
    denote
    denote-menu
    consult-denote
    denote-journal
    denote-org
    denote-sequence
    denote-silo
    citar-embark
    citar-denote
    denote-explore
    ox-epub
    (auctex :fetcher github :repo "emacs-straight/auctex" :branch "master")
    cdlatex))

(defvar *package-ai-install-list*
  (append '((gptel :fetcher github
                   :repo "karthink/gptel")
            gptel-magit
            (ragmacs :fetcher github :repo "positron-solutions/ragmacs")
            (gptel-agent :host github :repo "karthink/gptel-agent"
                         :files (:defaults "agents"))
            (gptel-aibo :fetcher github
                        :repo "dolmens/gptel-aibo")
            (agental :fetcher github :repo "lizqwerscott/agental"
                     :files (:defaults "prompts"))
            (mcp :fetcher github
                 :repo "lizqwerscott/mcp.el")
            (macher :host github :repo "kmontag/macher"))
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
 (append *package-built-in-install-list*
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
