;;; init-writer.el --- writer tool                   -*- lexical-binding: t; -*-

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

(require 'ews)

;;; nov
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; bixtex
(setopt bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                      ("file"     "Relative or absolute path to attachments" "" ))
        bibtex-align-at-equal-sign t)

(with-eval-after-load 'bibtex
  (ews-bibtex-register))

(global-bind-keys
 ("C-c n b r" . ews-bibtex-register))

;;; biblio
(global-bind-keys
 ("C-c n b b" . ews-bibtex-biblio-lookup))

;;; citar
(setopt citar-bibliography ews-bibtex-files)

(with-eval-after-load 'citar
  (require 'citar-nerd-icons)
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons)))

(global-bind-keys
 ("C-c n b o" . citar-open))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(citar-embark-mode)

;;; denote
(setopt denote-sort-keywords t
        denote-link-description-function #'ews-denote-link-description-title-case
        denote-rename-buffer-mode 1)

;; (add-hook 'dired-mode-hook
;;           #'denote-dired-mode)

(global-bind-keys
 ("C-c n d b" . denote-find-backlink)
 ("C-c n d d" . denote-date)
 ("C-c n d l" . denote-find-link)
 ("C-c n d i" . denote-link-or-create)
 ("C-c n d k" . denote-rename-file-keywords)
 ("C-c n d n" . denote)
 ("C-c n d r" . denote-rename-file)
 ("C-c n d R" . denote-rename-file-using-front-matter)
 ("C-c n d m" . denote-menu-list-notes))

;;; denote-org
(global-bind-keys
 ("C-c n d h" . denote-org-link-to-heading))

(consult-notes-denote-mode)

;;; consult-denote
(with-eval-after-load 'consult-denote
  (consult-denote-mode 1))

(global-bind-keys
 ("C-c n f" . consult-denote-find)
 ("C-c n g" . consult-denote-grep))

;;; citar-denote
(setopt citar-open-always-create-notes t
        citar-denote-subdir "literature")
(citar-denote-mode)

(global-bind-keys
 ("C-c n b c" . citar-create-note)
 ("C-c n b n" . citar-denote-open-note)
 ("C-c n b x" . citar-denote-nocite))

(keymap-binds org-mode-map
  ("C-c n b k" . citar-denote-add-citekey)
  ("C-c n b K" . citar-denote-remove-citekey)
  ("C-c n b d" . citar-denote-dwim)
  ("C-c n b e" . citar-denote-open-reference-entry))

;;; denote-explore
(defconst denote-id-regexp "@@\\([^.]*?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the IDENTIFIER field in a file name.")

(global-bind-keys
 ;; Statistics
 ("C-c n x c" . denote-explore-count-notes)
 ("C-c n x C" . denote-explore-count-keywords)
 ("C-c n x b" . denote-explore-barchart-keywords)
 ("C-c n x e" . denote-explore-barchart-filetypes)
 ;; Random walks
 ("C-c n x r" . denote-explore-random-note)
 ("C-c n x l" . denote-explore-random-link)
 ("C-c n x k" . denote-explore-random-keyword)
 ("C-c n x x" . denote-explore-random-regex)
 ;; Denote Janitor
 ("C-c n x d" . denote-explore-identify-duplicate-notes)
 ("C-c n x z" . denote-explore-zero-keywords)
 ("C-c n x s" . denote-explore-single-keywords)
 ("C-c n x o" . denote-explore-sort-keywords)
 ("C-c n x w" . denote-explore-rename-keyword)
 ;; Visualise denote
 ("C-c n x n" . denote-explore-network)
 ("C-c n x v" . denote-explore-network-regenerate)
 ("C-c n x D" . denote-explore-barchart-degree))

(provide 'init-writer)
;;; init-writer.el ends here
