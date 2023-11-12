;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defun package-check-install (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package))))

(package-check-install '(quelpa
                         ;; quelpa-use-package
                         ))

(setq quelpa-update-melpa-p nil)
;; (quelpa-use-package-activate-advice)

;; (update-load-path)
(defun site-lisp-update ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Update site lisp*")))
    (async-shell-command
     (concat "cd "
             user-emacs-directory
             " && git submodule foreach git pull")
     output-buffer)
    (switch-to-buffer-other-window output-buffer)))

;; (use-package quelpa
;;   :ensure t
;;   :custom
;;   (quelpa-update-melpa-p nil)
;;   :config
;;   (use-package quelpa-use-package
;;     :ensure t)
;;   (quelpa-use-package-activate-advice))

(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (site-lisp-update)
  (when (version<= "29" emacs-version)
    (package-upgrade-all))
  (quelpa-upgrade-all))

;;; install all package

(defvar *package-need-install-list*
  '(emacsql-sqlite-builtin
    no-littering
    benchmark-init
    gcmh
    vertico
    marginalia
    consult
    orderless
    pinyinlib
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
    highlight-indent-guides
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
    pretty-mode
    diredfl
    dired-subtree
    doom-themes
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
    immersive-translate))

(package-check-install *package-need-install-list*)

;; (when sys/linuxp
;;   (package-check-install
;;    '(tree-sitter
;;      tree-sitter-langs)))

(quelpa '(lazy-load :fetcher github
                    :repo "manateelazycat/lazy-load"))

(quelpa '(one-key :fetcher github :repo "manateelazycat/one-key"))

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

(quelpa '(vue-ts-mode
          :fetcher github
          :repo "8uff3r/vue-ts-mode"))

(quelpa '(sdcv
          :fetcher github
          :repo "manateelazycat/sdcv"))

(quelpa '(pyim-tsinghua-dict
          :fetcher github
          :repo "redguardtoo/pyim-tsinghua-dict"
          :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim")))

(provide 'init-package)
;;; init-package.el ends here
