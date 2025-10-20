;;; lib-enlight.el --- lib enlight                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'enlight-menu)
(require 'enlight)

(require 'grid)

(require 'nerd-icons)

(require 'recentf)

(defface enlight-yellow-bold
  '((t (:foreground "#cabf00" :bold t)))
  "Yellow bold face."
  :group 'enlight)

(defface enlight-puper-bold
  '((t (:foreground "#ff78c6" :bold t)))
  "Puper bold face."
  :group 'enlight)

(defvar enlight-emacs-logo
  (propertize
   "
███████╗███╗   ███╗ █████╗  ██████╗███████╗
██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
█████╗  ██╔████╔██║███████║██║     ███████╗
██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"
   'face 'enlight-puper-bold))

;; (defvar enlight-calendar
;;   (progn
;;     (calendar)
;;     (prog1 (with-current-buffer (buffer-name (current-buffer))
;; 	         (buffer-string))
;;       (calendar-exit))))

(defvar enlight-separator-line
  (propertize
   (concat (make-string 2 ?\u00B7)
           " "
           (make-string 20 #x2500)
           " "
           (make-string 2 ?\u00B7)
           " "
           (make-string 20 #x2500)
           " "
           (make-string 2 ?\u00B7))
   'face 'font-lock-comment-face))

;;; from dasboard package
(defun enlight-dashboard-init--packages-count ()
  "Get the intalled package count depending on package manager.
Supported package managers are: package.el, straight.el and elpaca.el."
  (let* ((package-count (if (bound-and-true-p package-alist)
                            (length package-activated-list)
                          0))
         (straight-count (if (boundp 'straight--profile-cache)
                             (hash-table-count straight--profile-cache)
                           0))
         (elpaca-count (if (fboundp 'elpaca--queued)
                           (length (elpaca--queued))
                         0)))
    (+ package-count straight-count elpaca-count)))

(defun shorten-file-path (file-path max-length)
  "Shorten the file path FILE-PATH to a length not exceeding MAX-LENGTH.
Keep the beginning directory and filename, and display only the first letter of
each directory in the middle."
  (let* ((dir-part (file-name-directory file-path))
         (file-name (file-name-nondirectory file-path))
         (dir-components (when dir-part
                           (split-string (directory-file-name dir-part) "/" t))))
    (if (<= (length file-path) max-length)
        (cons dir-part file-name)
      (let ((base-dir (car dir-components))
            (remaining-dirs (cdr dir-components)))
        (if (null remaining-dirs)
            (cons dir-part file-name)
          ;; 将中间目录转换为首字母缩写
          (let* ((abbreviated-dirs (mapcar (lambda (dir)
                                             (if (> (length dir) 0)
                                                 (substring dir 0 1)
                                               ""))
                                           remaining-dirs))
                 (middle-path (string-join abbreviated-dirs "/"))
                 (shortened-dir (concat base-dir "/" middle-path "/")))
            (if (<= (length (concat shortened-dir file-name)) max-length)
                (cons shortened-dir file-name)
              (let* ((base-length (+ (length base-dir) (length file-name) 2))
                     (available-length (- max-length base-length))
                     (dirs-to-keep (floor (/ available-length 2)))
                     (kept-dirs (if (> dirs-to-keep 0)
                                    (seq-take abbreviated-dirs dirs-to-keep)
                                  '()))
                     (final-middle (if kept-dirs
                                       (concat (string-join kept-dirs "/") "/...")
                                     "...")))
                (cons (concat base-dir "/" final-middle "/")
                      file-name)))))))))

(defun enlight-recent-files-right ()
  "Get enlight recent files."
  (add-hook 'enlight-after-insert-hook #'enlight-menu-first-button)
  (let ((alist (cl-mapcar (lambda (f index)
                            (pcase-let* ((`(,file-dir . ,file-name) (shorten-file-path f 50)))
                              `(,(format "%s %s%s"
                                         (nerd-icons-icon-for-file f)
                                         (propertize
                                          file-dir
                                          'face 'font-lock-comment-face)
                                         file-name)
                                (find-file ,f)
                                ,(format "C-%d"
                                         index))))
                          (seq-take recentf-list 5)
                          (number-sequence 1 5))))
    (enlight-menu--apply-keys (list
                               `("Recent files"
                                 ,@alist)))
    (grid-make-column
     `(,(grid-make-box `(:align left :content ,(concat "Recent "
                                                       (propertize
                                                        "files:"
                                                        'face 'enlight-puper-bold))
                                :width 0.5))
       ,(grid-make-row
         (list
          (grid-make-column
           (mapcar (lambda (item)
                     (grid-make-row
                      `((
                         :align left
                         :content ,(car item)
                         ;; (propertize (car item)
                         ;;             'menu-id (cl-incf enlight-menu-count)
                         ;;             'action (enlight--normalize-command (elt item 1))
                         ;;             'cursor-face 'enlight-menu-selected-face
                         ;;             'mouse-face 'enlight-menu-selected-face)
                         :width 0.4))))
                   alist))
          (grid-make-column
           (mapcar (lambda (item)
                     (grid-make-row
                      `((
                         :align right
                         :content ,(propertize (format "[%s]"
                                                       (elt item 2))
                                               'face 'enlight-menu-key)
                         :width 0.1))))
                   alist))))))))


(defun enlight-recent-files-left ()
  "Get enlight recent files."
  (add-hook 'enlight-after-insert-hook #'enlight-menu-first-button)
  (let ((alist (cl-mapcar (lambda (f index)
                            (pcase-let* ((`(,file-dir . ,file-name) (shorten-file-path f 100)))
                              `(,(format "%s %s%s"
                                         (nerd-icons-icon-for-file f)
                                         (propertize
                                          file-dir
                                          'face 'font-lock-comment-face)
                                         file-name)
                                (find-file ,f)
                                ,(format "C-%d"
                                         index))))
                          (seq-take recentf-list 5)
                          (number-sequence 1 5))))
    (enlight-menu--apply-keys (list
                               `("Recent files"
                                 ,@alist)))
    `(,(grid-make-box `(:align left :content ,(concat "Recent "
                                                      (propertize
                                                       "files:"
                                                       'face 'enlight-puper-bold))
                               :width 0.5))
      "\n"
      ,@(apply #'append
               (cl-mapcar (lambda (item)
                            (pcase-let ((`(,desc ,_ ,shortkey) item))
                              (list
                               (grid-make-box
                                `(
                                  :align left
                                  :content ,(format "%s %s"
                                                    (propertize (format "[%s]"
                                                                        shortkey)
                                                                'face 'enlight-menu-key)
                                                    desc)
                                  :width 0.5))
                               "\n")))
                          alist)))))

(defun enlight-init-info ()
  "Enlight init info."
  (concat
   (propertize
    (number-to-string (enlight-dashboard-init--packages-count))
    'face 'enlight-yellow-bold)
   " "
   (propertize
    "个包被安装."
    'face 'font-lock-comment-face)
   " "
   (propertize
    "启动时长:"
    'face 'font-lock-comment-face)
   " "
   (propertize
    (format "%s" (emacs-init-time))
    'face 'enlight-yellow-bold)))

(provide 'lib-enlight)
;;; lib-enlight.el ends here
