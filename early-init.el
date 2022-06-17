;;;; early-init.el --- Early initialization. -*- lexical-binding:  -*-

;; This file is not part of GNU Emacs.
;;

;;; Code:

;; Defer garbage collection further back in the startup proces
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

