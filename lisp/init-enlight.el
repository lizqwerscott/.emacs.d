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
              ("Calendar" calendar "C")
              ("Tasks file" (find-file "~/Documents/Org/tasks.org") "T")
              ("Denote Menu" denote-menu-list-notes "J"))))
         "   "
         ,(enlight-menu
           `(("Jump Config"
              ("Custom file"
               (find-file ,custom-file)
               "S")
              ("Init file"
               (find-file ,user-init-file)
               "j")
              ("Config project"
               (project-switch-project user-emacs-directory)
               "c")
              ("Package" elpaca-manager "P")
              ("Restart" restart-emacs "R"))))
         "   "
         ,(enlight-menu
           `(("Quick"
              ("Projects" project-switch-project "p")
              ("Recents" consult-recent-file "r")
              ("Dirs" consult-dir "d")
              ("Notes" consult-notes "n")
              ("Bookmarks" consult-bookmark "b"))))))
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
