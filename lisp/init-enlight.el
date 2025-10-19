;;; init-enlight.el ---                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defface enlight-yellow-bold
  '((t (:foreground "#cabf00" :bold t)))
  "Yellow bold face.")

(defface enlight-puper-bold
  '((t (:foreground "#ff78c6" :bold t)))
  "Yellow bold face.")

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
   (concat ". . "
           (make-string 20 #x2500)
           " .. "
           (make-string 20 #x2500)
           " . .")
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

(defun enlight-init-info ()
  "Enlight init info."
  (propertize
   (concat (format "%s 个包被安装." (enlight-dashboard-init--packages-count))
           " "
           (format "启动时长: %s" (emacs-init-time)))
   'face 'font-lock-comment-face))

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
       ,(grid-make-row
         `(,(enlight-menu
             `(("Recent files:"
                ,@(cl-mapcar (lambda (f index)
                               `(,(format "%s. %s" index f)
                                 (find-file ,f)
                                 ,(format "C-%d"
                                          index)))
                             (seq-take recentf-list 5)
                             (number-sequence 1 5)))))))
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
