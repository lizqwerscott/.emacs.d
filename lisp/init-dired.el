;;; init-dired.el --- dired                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib-dired)

;;; dired
(require 'dired)
(setq delete-by-moving-to-trash t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-auto-revert-buffer t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-dwim-target t)
(setq dired-listing-switches "-AFhlv --group-directories-first")
;; (setq dired-listing-switches
;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
(unless user/dirvish
  (setq dired-movement-style 'bounded)
  (setq dired-kill-when-opening-new-dired-buffer t))

;; Dont prompt about killing buffer visiting delete file
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-guess-shell-alist-user
      `((,(rx "."
              (or
               ;; Videos
               "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
               ;; Music
               "wav" "mp3" "flac"
               ;; Images
               "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
               ;; Docs
               "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
              string-end)
         ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                ((eq system-type 'darwin) "open")
                ((eq system-type 'windows-nt) "start")
                (t "")))))

(setq ls-lisp-dirs-first t)
(when sys/macp
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

(advice-add 'find-file :around 'find-file-auto)

;;; dired-aux
(setopt dired-isearch-filenames 'dwim
        dired-create-destination-dirs 'ask
        dired-vc-rename-file t)

;;; dired-x
(setopt dired-bind-vm nil
        dired-bind-man nil
        dired-bind-info nil
        dired-omit-verbose nil
        dired-omit-files (rx string-start
                             (or ".DS_Store"
                                 ".cache"
                                 ".vscode"
                                 ".ccls-cache" ".clangd")
                             string-end)
        dired-omit-files (concat dired-omit-files "\\|^\\..*$"))

;;; dired-subtree
(setopt dired-subtree-use-backgrounds nil)

(keymap-binds dired-mode-map
  ("<tab>" . dired-subtree-toggle)
  ("TAB" . dired-subtree-toggle)
  ;; ("<backtab>" . dired-subtree-remove)
  ;; ("S-TAB" . dired-subtree-remove)
  )

;;; dired-sort
(dired-quick-sort-setup)

;;; trashed
(setopt trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%Y-%m-%d %H:%M:%S")

(global-bind-keys
 ("C-x C-t" . trashed))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
              (dired-hide-details-mode)
              (dired-async-mode)
              (unless (bound-and-true-p dirvish-override-dired-mode)
                (nerd-icons-dired-mode))
              (dired-omit-mode)))

(add-hook 'dired-mode-hook
          (lambda ()
            (when (cl-find (file-truename default-directory)
                           (mapcar #'file-truename
                                   '("~/Downloads/"))
                           :test #'string=)
              (dired-auto-sort-dir)))
          90)

(diredfl-global-mode)

(require 'lib-transient)
(require 'casual-dired)

;;; Menu
(pretty-transient-define-prefix dired-dispatch ()
  "Dired dispatch menu"
  [["Directory"
    ("h" "Hide Details" dired-hide-details-mode :toggle t :transient t)
    ("o" "Omit Mode" dired-omit-mode :toggle t :transient t)]
   ["Highlight"
    ("f" "Diredfl" diredfl-mode :toggle t :transient t)
    ("d" "Denote dired" denote-dired-mode :toggle t :transient t)]
   ["Sort By"
    ("n" "Name" casual-dired--sort-by-name :transient t)
    ("k" "Kind" casual-dired--sort-by-kind :transient t)
    ("l" "Date Last Opened" casual-dired--sort-by-date-last-opened
     :transient t)
    ("a" "Date Added" casual-dired--sort-by-date-added :transient t)
    ("m" "Date Modified" casual-dired--sort-by-date-modified :transient t)
    ("M" "Date Metadata Changed" casual-dired--sort-by-date-metadata-changed
     :transient t)
    ("s" "Size" casual-dired--sort-by-size :transient t)]]
  [("q" "Quit" transient-quit-all)])

;;; Keymap
(keymap-binds dired-mode-map
  ("e" . dired-toggle-read-only)
  ("f" . (lambda ()
           (interactive)
           (consult-fd default-directory)))
  ("F" . consult-fd-dir)
  ("W" . dired-copy-path)
  ("C-c +" . dired-create-empty-file)
  ("C-+" . dired-create-empty-file)
  ("h" . dired-up-directory)
  ("C-c C-r" . dired-rsync)
  ("C-c C-x" . dired-rsync-transient)
  ("C-c e" . dired-do-open-default)
  (("C-j" "M-j" "s-j") . dired-other-window)
  ("C-o" . dired-dispatch)

  ("/" . prot-dired-limit-regexp)
  ("C-c C-l" . prot-dired-limit-regexp)

  (("M-n" "s-n") . prot-dired-subdirectory-next)
  (("M-p" "s-p") . prot-dired-subdirectory-previous)
  ("C-c C-n" . prot-dired-subdirectory-next)
  ("C-c C-p" . prot-dired-subdirectory-previous))

(global-bind-keys
 ("C-x J" . dired-jump-other-window)
 ("C-x x e" . dired-do-open-default))

;;; dirvish
(when user/dirvish
  (with-eval-after-load 'dirvish
    (setq dirvish-attributes
          '(nerd-icons file-time file-size collapse subtree-state vc-state))

    (custom-set-variables
     '(dirvish-quick-access-entries ; It's a custom option, `setq' won't work
       '(("h" "~/"                          "Home")
         ("d" "~/Downloads/"                "Downloads")
         ("D" "~/Documents/"                "Documents")
         ("p" "~/MyProject/"                "MyProject")
         ("g" "~/github/"                   "Github")
         ("m" "/mnt/"                       "Drives")
         ("t" "~/.local/share/Trash/files/" "TrashCan"))))

    (keymap-binds dirvish-mode-map
      ("a" . dirvish-quick-access)
      ("TAB" . dirvish-subtree-toggle)
      (("M-i" "s-i") . dirvish-layout-toggle)
      ("C-j" . dirvish-fd)))

  (dirvish-override-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
