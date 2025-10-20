;;; init-enlight.el ---                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib-enlight)

(setopt enlight-center-vertically nil)

(custom-set-variables
 `(enlight-content
   (concat
    (grid-make-box `( :align center :content ,enlight-emacs-logo :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,(enlight-init-info) :width 0.5))
    "\n\n"
    ;; enlight-calendar "\n"
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
      :width 0.5))
    "\n"
    (grid-make-box `( :align center :content ,enlight-separator-line :width 0.5))
    "\n"
    ;; (grid-make-box
    ;;  (:align center :content ,(enlight-recent-files-right) :width 0.5))
    ,@(enlight-recent-files-left)
    )))

(global-bind-keys
 ("<f2>" . enlight-open))

(provide 'init-enlight)
;;; init-enlight.el ends here
