;;; dired
(require 'dired)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
;; (setq dired-recursive-deletes 'always)
(setq dired-auto-revert-buffer t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-dwim-target t)
(setq dired-listing-switches "-AFhlv --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)

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

;; (setq dired-listing-switches
;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;;; dired-aux
(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)

;;; dired-x
(require 'dired-x)
(setq dired-bind-vm nil)
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                              ".cache"
                              ".vscode"
                              ".ccls-cache" ".clangd")
                           string-end))
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*$"))

;;; dired-subtree
(require 'dired-subtree)

;;; dired-sort
(require 'dired-quick-sort)
(dired-quick-sort-setup)

;;; Find file auto
(defcustom user/find-file-auto-regexp-list '("\\.xlsx?\\'" "\\.pptx?\\'" "\\.docx?\\'" "\\.mp4\\'" "\\.app\\'")
  "Open files via external program regexp list  when using find-file "
  :group 'user
  :type 'list)

(defun find-file-auto (orig-fun &rest args)
  "Open files via external program when using find-file"
  (let ((file (car args)))
    (if (cl-find-if (lambda (regexp)
                      (string-match regexp file))
                    user/find-file-auto-regexp-list)
        (shell-command-do-open
         (list (file-truename file)))
      (apply orig-fun args))))
(advice-add 'find-file :around 'find-file-auto)

;;; Functions
(defun dired-copy-path ()
  "In dired, copy file path to kill-buffer.
At 2nd time it copy current directory to kill-buffer."
  (interactive)
  (when-let* ((path (dired-file-name-at-point)))
    (if (string= path (current-kill 0 1))
        (setq path (dired-current-directory)))
    (message path)
    (kill-new path)))

(defun dired-do-open-default ()
  "Open the current default directory using the external app."
  (interactive)
  (shell-command-do-open
   (list (file-truename default-directory))))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
              (unless (bound-and-true-p dirvish-override-dired-mode)
                (nerd-icons-dired-mode))
              (diredfl-mode)
              (dired-omit-mode)))

;;; Keymap
(keymap-sets dired-mode-map
             '(("TAB" . dired-subtree-cycle)
               ("e" . dired-toggle-read-only)
               ("f" . (lambda ()
                        (interactive)
                        (autoload 'consult-fd-dir "init-func" nil t)
                        (consult-fd-dir)))
               ("W" . dired-copy-path)
               ("C-c +" . dired-create-empty-file)
               ("M-n" . scroll-up-1/3)
               ("M-p" . scroll-down-1/3)
               ("h" . dired-up-directory)
               ("C-c C-r" . dired-rsync)
               ("C-c C-x" . dired-rsync-transient)
               ("C-c e" . dired-do-open-default)
               (("M-j" "s-j") . dired-other-window)))

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

    (when (executable-find "eza")
      (dirvish-define-preview eza (file)
        "Use `eza' to generate directory preview."
        :require ("eza") ; tell Dirvish to check if we have the executable
        (when (file-directory-p file) ; we only interest in directories here
          `(shell . ("eza" "-al" "--color=always" "--icons=always"
                     "--group-directories-first" ,file))))

      (setq dirvish-preview-dispatchers
            (cl-substitute 'eza 'dired dirvish-preview-dispatchers)))

    (keymap-sets dirvish-mode-map
                 '(("a" . dirvish-quick-access)
                   ("TAB" . dirvish-subtree-toggle)
                   (("M-i" "s-i") . dirvish-layout-toggle)
                   ("C-j" . dirvish-fd-jump))))

  (dirvish-override-dired-mode))

(provide 'init-dired)
