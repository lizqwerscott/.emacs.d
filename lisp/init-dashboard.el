;;; init-dashboard.el --- init emacs dashboard       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'nerd-icons)

(defvar *start-banner* (propertize ";;     *
;;      May the Code be with You!
;;     .                                 .
;;                               *
;;          /\\/|_      __/\\\\
;;         /    -\\    /-   ~\\  .              \\='
;;         \\    = Y =T_ =   /
;;          )==*(\\=`     \\=`) ~ \\
;;         /     \\     /     \\
;;         |     |     ) ~   (
;;        /       \\   /     ~ \\
;;        \\       /   \\~     ~/
;; _/\\_/\\_/\\__  _/_/\\_/\\__~__/_/\\_/\\_/\\_/\\_/\\_
;; |  |  |  | ) ) |  |  | ((  |  |  |  |  |  |
;; |  |  |  |( (  |  |  |  \\\\ |  |  |  |  |  |
;; |  |  |  | )_) |  |  |  |))|  |  |  |  |  |
;; |  |  |  |  |  |  |  |  (/ |  |  |  |  |  |
;; |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
\n" 'face '(:foreground "green")))

(defvar *start-image-banner*
  (find-image
   `(( :type png
       :file ,(expand-file-name "logo_black_medium.png" user-emacs-directory)))))

;; 自定义 *scratch* 内容
;;;###autoload
(defun +evan/scratch-setup()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer "*scratch*")
      ;; (erase-buffer)
      (insert *start-banner*)
      ;; (insert-image *start-image-banner* "Emacs")
      (insert "\n")
      (insert (format "启动时长: %s" (emacs-init-time)))
      (insert "\n")
      (insert-button "Quit Emacs"
                     'action (lambda (_button)
                               (save-buffers-kill-emacs)))
      (insert "\n")
      ;; (insert "Recent Files\n")
      ;; (dolist (f recentf-list)
      ;;   (insert-button f
      ;;                  'action (lambda (region)
      ;;                         (require 'f)
      ;;                         (let* ((f (buffer-substring-no-properties (overlay-start region) (overlay-end region)))
      ;;                                (fname (f-filename f)))
      ;;                           (find-file-noselect f)
      ;;                           (switch-to-buffer fname))))
      ;;   (insert "\n"))
      ))
  (goto-char (point-max)))

(add-hook 'dashboard-mode-hook
          #'page-break-lines-mode)

(with-eval-after-load 'page-break-lines
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

(custom-set-faces
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(dashboard-items-face ((t (:weight normal))))
 '(dashboard-no-items-face ((t (:weight normal)))))

(setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
      dashboard-startup-banner (or user/logo 'official)
      dashboard-image-banner-max-width 800
      dashboard-image-banner-max-height 300
      dashboard-path-max-length 60
      dashboard-path-style 'truncate-middle
      dashboard-page-separator "\n\f\n"
      dashboard-center-content t
      dashboard-vertically-center-content t
      dashboard-projects-backend 'project-el

      dashboard-items '((recents . 10)
                        ;; (bookmarks . 5)
                        (projects . 5)
                        ;; (agenda . 5)
                        )
      dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  ;; dashboard-insert-newline
                                  ;; dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer)

      dashboard-display-icons-p t
      dashboard-icon-type 'nerd-icons
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-heading-icons '((recents   . "nf-oct-history")
                                (bookmarks . "nf-oct-bookmark")
                                (agenda    . "nf-oct-calendar")
                                (projects  . "nf-oct-briefcase")
                                (registers . "nf-oct-database"))
      dashboard-footer-icon
      (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)

      dashboard-set-init-info t
      dashboard-set-footer t)

(with-eval-after-load 'dashboard
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (dashboard-insert-center
     (propertize (format "\nPowered by Lizqwer Scott, %s\n" (format-time-string "%Y"))
                 'face 'font-lock-comment-face)))
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright))

(defvar dashboard-recover-layout-p nil
  "Whether recovers the layout.")

(defun open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  ;; Check if need to recover layout
  (if (length> (window-list-1)
               ;; exclude `treemacs' window
               (if (and (fboundp 'treemacs-current-visibility)
                        (eq (treemacs-current-visibility) 'visible))
                   2
                 1))
      (setq dashboard-recover-layout-p t))

  ;; Display dashboard in maximized window
  (delete-other-windows)

  ;; Refresh dashboard buffer
  (dashboard-refresh-buffer))

(defun quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t)

  ;; Recover layout
  (and dashboard-recover-layout-p
       (and (bound-and-true-p winner-mode) (winner-undo))
       (setq dashboard-recover-layout-p nil)))

;;; menu
(pretty-hydra-define hydra-dashboard
  (:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-view_dashboard")
          :color pink :quit-key ("q" "C-g"))
  ("Navigator"
   (("P" elpaca-manager "package manage" :exit t)
    ("S" find-custom-file "settings" :exit t)
    ("I" find-init-file "init file" :exit t))
   "Item"
   (("RET" widget-button-press "open" :exit t)
    ("<tab>" widget-forward "next")
    ("C-i" widget-forward "next")
    ("<backtab>" widget-backward "previous")
    ("C-n" next-line "next line")
    ("C-p" previous-line "previous  line"))
   "Misc"
   (("<f2>" open-dashboard "open" :exit t)
    ("g" dashboard-refresh-buffer "refresh" :exit t)
    ("Q" quit-dashboard "quit" :exit t))))

(global-bind-keys
 ("<f2>" . open-dashboard))

(with-eval-after-load 'dashboard
  (keymap-binds dashboard-mode-map
    ("S" . find-custom-file)
    ("I" . find-init-file)
    ("P" . elpaca-manager)
    ("q" . quit-dashboard)
    ("h" . hydra-dashboard/body)
    ("?" . hydra-dashboard/body)))

(pcase user/dashboard
  ('dashboard
   (dashboard-setup-startup-hook))
  ('scratch
   (add-hook 'after-init-hook
             #'+evan/scratch-setup))
  ('enlight
   (require 'init-enlight)
   (setopt initial-buffer-choice #'enlight)))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
