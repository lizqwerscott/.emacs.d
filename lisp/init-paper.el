;;; init-paper.el --- init about paper packages      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords:

;;; Commentary:

;;; Code:

(use-package ebib
  :ensure t
  :config
  (setq ebib-preload-bib-files
      '("~/Documents/Sync/paper/bib/references.bib"))
  (setq ebib-bib-search-dirs
      "~/Documents/Sync/paper/bib/"))

(use-package citar
  :ensure t
  :custom
  (org-cite-global-bibliography '("~/Documents/Sync/paper/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths '("~/Documents/Sync/paper/pdf/"))
  (citar-notes-paths '("~/Documents/Sync/paper/notes/"))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  ;; (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  )

(provide 'init-paper)
;;; init-paper.el ends here.
