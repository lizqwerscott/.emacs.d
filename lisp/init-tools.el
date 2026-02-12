;;; init-tools.el --- tools                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; which-key

(setq which-key-max-description-length 30
      which-key-lighter nil
      which-key-show-remaining-keys t
      which-key-sort-order 'which-key-description-order)

(which-key-mode)

(with-eval-after-load 'eww
  (keymap-binds eww-mode-map
    ("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)))

;;; gif screenshot
(with-eval-after-load 'gif-screencast
  (keymap-binds gif-screencast-mode-map
    ("<f8>" . gif-screencast-toggle-pause)
    ("<f9>" . gif-screencast-stop)))

;;; proced
(setq proced-auto-update-flag 'visible
      proced-auto-update-interval 3
      proced-enable-color-flag t)

;;; fuzzy clock
(require 'fuzzy-clock-zh)
(setq fuzzy-clock-zh-fuzziness 'fifteen-minutes)
(fuzzy-clock-zh-mode 1)

;;; detached

(require 'detached)

(detached-init)

(connection-local-set-profile-variables
 'remote-detached
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "~/.detached/sessions")))

(connection-local-set-profiles '(:application tramp :protocol "ssh") 'remote-detached)

;;; bookmark

;; Display icons for bookmarks
(defun my-bookmark-bmenu--revert ()
  "Re-populate `tabulated-list-entries'."
  (let (entries)
    (dolist (full-record (bookmark-maybe-sort-alist))
      (let* ((name       (bookmark-name-from-full-record full-record))
             (annotation (bookmark-get-annotation full-record))
             (location   (bookmark-location full-record))
             (file       (file-name-nondirectory location))
             (type       (let ((fmt "%-8.8s"))
                           (cond ((null location)
                                  (propertize (format fmt "NOFILE") 'face 'warning))
                                 ((file-remote-p location)
                                  (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                 ((not (file-exists-p location))
                                  (propertize (format fmt "NOTFOUND") 'face 'error))
                                 ((file-directory-p location)
                                  (propertize (format fmt "DIRED") 'face 'warning))
                                 (t (propertize (format fmt "FILE") 'face 'success)))))
             (icon       (cond
                          ((file-remote-p location)
                           (nerd-icons-codicon "nf-cod-radio_tower"))
                          ((file-directory-p location)
                           (nerd-icons-icon-for-dir location))
                          ((not (string-empty-p file))
                           (nerd-icons-icon-for-file file)))))
        (push (list
               full-record
               `[,(if (and annotation (not (string-equal annotation "")))
                      "*" "")
                 ,icon
                 ,(if (display-mouse-p)
                      (propertize name
                                  'font-lock-face 'bookmark-menu-bookmark
                                  'mouse-face 'highlight
                                  'follow-link t
                                  'help-echo "mouse-2: go to this bookmark in other window")
                    name)
                 ,type
                 ,@(if bookmark-bmenu-toggle-filenames
                       (list (propertize location 'face 'completions-annotations)))])
              entries)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries entries))
  (tabulated-list-print t))
(advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

(defun my-bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (if (called-interactively-p 'interactive)
        (pop-to-buffer buf)
      (set-buffer buf)))
  (bookmark-bmenu-mode)
  (bookmark-bmenu--revert))
(advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

(define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("" 1) ;; Space to add "*" for bookmark with annotation
          ("" 2) ;; Icons
          ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
          ("Type" 9)
          ,@(if bookmark-bmenu-toggle-filenames
                '(("File" 0 bookmark-bmenu--file-predicate)))])
  (setq tabulated-list-padding bookmark-bmenu-marks-width)
  (setq tabulated-list-sort-key '("Bookmark" . nil))
  (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
  (setq revert-buffer-function #'bookmark-bmenu--revert)
  (tabulated-list-init-header))

(provide 'init-tools)
;;; init-tools.el ends here
