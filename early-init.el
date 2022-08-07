;;;; early-init.el --- Early initialization. -*- lexical-binding:  -*-

;; This file is not part of GNU Emacs.
;;

;;; Code:

;; Defer garbage collection further back in the startup proces
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; (setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
