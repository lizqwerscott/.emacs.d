;;; init-elfeed.el --- elfeed                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages!
 '(elfeed
   elfeed-org))

(with-eval-after-load 'elfeed
  ;; Load elfeed-org
  (require 'elfeed-org)

  ;; Initialize elfeed-org
  ;; This hooks up elfeed-org to read the configuration when elfeed
  ;; is started with =M-x elfeed=
  (elfeed-org)

  ;; Optionally specify a number of files containing elfeed
  ;; configuration. If not set then the location below is used.
  ;; Note: The customize interface is also supported.
  (setopt rmh-elfeed-org-files (list "~/Documents/Org/elfeed.org"))

  (setopt url-queue-timeout 30)

  (require 'lib-elfeed)
  (setopt elfeed-search-print-entry-function #'lucius/elfeed-search-print-entry--better-default))

(autoload #'tab-bar-switch-or-create-rss "lib-elfeed" nil t)

(global-bind-keys
 ("C-c l r" . ("Rss Tab" . tab-bar-switch-or-create-rss)))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
