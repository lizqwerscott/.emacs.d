;;; init-enlight.el ---                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defface enlight-yellow-bold
  '((t (:foreground "#cabf00" :bold t)))
  "Yellow bold face.")

(defface enlight-puper-bold
  '((t (:foreground "#ff78c6" :bold t)))
  "Puper bold face.")

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

(require 'grid)

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

(defun enlight-recent-files ()
  "Get enlight recent files."
  (require 'enlight-menu)
  (require 'nerd-icons)
  (add-hook 'enlight-after-insert-hook #'enlight-menu-first-button)
  (let ((alist (cl-mapcar (lambda (f index)
                            (let ((file-dir (file-name-directory f))
                                  (file-name (file-name-nondirectory f)))
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

(setopt enlight-center-vertically nil)

(custom-set-variables
 '(enlight-content
   (concat
    (grid-make-box `( :align center :content ,enlight-emacs-logo :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,(enlight-init-info) :width 0.5))
    "\n\n"
    ;; enlight-calendar "\n"
    (grid-make-box `( :align center :content ,enlight-separator-line :width 0.5))
    "\n"
    (grid-make-box
     `(
       :content
       ,(enlight-recent-files)
       :align left
       :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,enlight-separator-line :width 0.5))
    "\n"
    (grid-make-box
     (list
      :content
      (grid-make-row
       `(,(enlight-menu
           '(("Agenda"
              ("Agenda Week" (org-agenda nil "a") "a")
              ("Agenda Todo" (org-agenda nil "t") "t")
              ("Calendar" calendar "C"))))
         "   "
         ,(enlight-menu
           `(("Jump Config"
              ("Init file"
               (find-file ,user-init-file)
               "j")
              ("Config project"
               (project-switch-project user-emacs-directory)
               "c")
              ("Package" elpaca-manager "P"))))
         "   "
         ,(enlight-menu
           `(("Quick"
              ("Projects" project-switch-project "p")
              ("Recents" consult-recent-file "r")
              ("Dir" consult-dir "d"))))))
      :align 'center
      :width 0.5)))))

(global-bind-keys
 ("<f2>" . enlight-open))

(provide 'init-enlight)
;;; init-enlight.el ends here
