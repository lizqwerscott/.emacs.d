;;; Custom
(require 'dired)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
;; (setq dired-recursive-deletes 'always)
(setq dired-bind-vm nil)
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq dired-auto-revert-buffer t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-AFhlv")

(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)

(require 'dired-x)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                              ".cache"
                              ".vscode"
                              ".ccls-cache" ".clangd")
                           string-end))
;; (setq dired-omit-files
;;       (concat dired-omit-files "\\|^\\..*$"))
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
(require 'dired-subtree)


;; (setq dired-listing-switches
;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;;; Functions
;; open files via external program based on file types, See:
;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
(defun xdg-open (filename)
  (interactive "fFilename: ")
  (let ((process-connection-type))
    (start-process "" nil (cond ((eq system-type 'gnu/linux) "xdg-open")
                              ((eq system-type 'darwin) "open")
                              ((eq system-type 'windows-nt) "start")
                              (t "")) (expand-file-name filename))))
;; open files via external program when using find-file
(defun find-file-auto (orig-fun &rest args)
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         '(
           ;; "\\.html?\\'"
           "\\.xlsx?\\'"
           "\\.pptx?\\'"
           "\\.docx?\\'"
           "\\.mp4\\'"
           "\\.app\\'"
           ))
        (xdg-open filename)
      (apply orig-fun args))))
(advice-add 'find-file :around 'find-file-auto)

(defun dired-copy-path ()
  "In dired, copy file path to kill-buffer.
At 2nd time it copy current directory to kill-buffer."
  (interactive)
  (let (path)
    (setq path (dired-file-name-at-point))
    (if (string= path (current-kill 0 1)) (setq path (dired-current-directory)))
    (message path)
    (kill-new path)))

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (or (string-equal major-mode "dired-mode")
                   (string-equal major-mode "dirvish-mode"))
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
              (nerd-icons-dired-mode)
              (diredfl-mode)
              (dired-omit-mode)))

;;; Keymap
(keymap-sets dired-mode-map
             '(("TAB" . dired-subtree-cycle)
               ("f" . consult-fd-dir)
               ("C-<return>" . xah-open-in-external-app)
               ("C-c +" . dired-create-empty-file)))

;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-mode-line-format
;;    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   ;; (dirvish-attributes '(subtree-state all-the-icons vc-state git-msg))
;;   (dirvish-attributes '(all-the-icons vc-state file-size))
;;   (dirvish-preview-dispatchers '(vc-diff))
;;   (dirvish-mode-line-height 0)
;;   (dirvish-header-line-height 0)
;;   :bind
;;   (:map dirvish-mode-map
;;         ("/" . dirvish-fd)
;;         ("a"   . dirvish-quick-access)
;;         ("f"   . dirvish-file-info-menu)
;;         ("y"   . dirvish-yank-menu)
;;         ("N"   . dirvish-narrow)
;;         ("^"   . dirvish-history-last)
;;         ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;         ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;         ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;         ("TAB" . dirvish-subtree-toggle)
;;         ("h" . dired-up-directory)
;;         ("q" . my/meow-quit))
;;   :config
;;   (dirvish-peek-mode))

(provide 'init-dired)
