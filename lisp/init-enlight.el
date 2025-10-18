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

(custom-set-variables
 '(enlight-content
   (concat
    (grid-make-box `( :align center :content ,enlight-emacs-logo :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,(format "启动时长: %s" (emacs-init-time))
                      :width 0.5))
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
                               `(,(format "%s" f)
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
     `(:content
       ,(grid-make-row
         `(,(enlight-menu
             `(("Quick dir:"
                ,@(mapcar (lambda (dir)
                            `(,dir (dired ,dir)))
                          user/quickdir))))))
       :align left
       :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,enlight-separator-line :width 0.5))
    "\n"
    (grid-make-box
     (list
      :content (grid-make-row
                `(,(enlight-menu
                    '(("Main"
                       ("Org-Agenda (current day)" (org-agenda nil "a") "a")
                       ("Projects" project-switch-project "p")
                       ("Recents" consult-recent-file "r"))))
                  "   "
                  ,(enlight-menu
                    `(("Emacs Config"
                       ("Jump to the config"
                        (find-file ,user-init-file)
                        "j")
                       ("Jump to the config project"
                        (project-switch-project user-emacs-directory)
                        "c"))))
                  ))
      :align 'left
      :wdith 0.5)))))

(provide 'init-enlight)
;;; init-enlight.el ends here
