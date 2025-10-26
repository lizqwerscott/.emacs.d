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
 ("C-c n q r" . ews-bibtex-register))

;;; biblio
(global-bind-keys
 ("C-c n q b" . ews-bibtex-biblio-lookup))

;;; citar
(setopt citar-bibliography ews-bibtex-files)

(with-eval-after-load 'citar
  (require 'citar-nerd-icons)
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons)))

(global-bind-keys
 ("C-c n q o" . citar-open))

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

(setopt denote-templates
        '((week-report . denote-week-report-template)))

(with-eval-after-load 'denote
  ;; for fix `denote--file-has-backlinks-p', use old.
  (advice-add #'denote--file-has-backlinks-p
              :around
              (lambda (_ &rest args)
                (not (zerop (length (denote-get-backlinks (car args))))))))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "新笔记 (使用 Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(add-hook 'dired-mode-hook
          (lambda ()
            (when (file-in-directory-p default-directory denote-directory)
              (diredfl-mode -1)
              (denote-dired-mode))))

(global-bind-keys
 ("C-c n n" . denote)
 ("C-c n N" . denote-date)

 ("C-c n d" . denote-sort-dired)
 ("C-c n r" . denote-rename-file)

 ("C-c n i" . denote-link-or-create)
 ("C-c n I" . denote-add-links)

 ("C-c n l" . denote-find-link)

 ("C-c n b" . denote-backlinks)
 ("C-c n R" . denote-rename-file-using-front-matter))

;;; denote menu
(global-bind-keys
 ("C-c n m" . denote-menu-list-notes))

;;; denote-org
(global-bind-keys
 ("C-c n h" . denote-org-link-to-heading))

;; for Hugo export
(with-eval-after-load 'denote
  (advice-add 'denote-link-ol-export :around
              (lambda (orig-fun link description format)
                (if (and (eq format 'md)
                         (eq org-export-current-backend 'hugo))
                    (let* ((path (denote-get-path-by-id link))
                           (export-file-name
                            (or
                             ;; Use export_file_name if it exists
                             (when (file-exists-p path)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (goto-char (point-min))
                                 (when (re-search-forward "^#\\+export_file_name: \\(.+\\)" nil t)
                                   (match-string 1))))
                             ;; Otherwise, use the original file's base name
                             (file-name-nondirectory path)))
                           (ext (file-name-extension export-file-name)))
                      (if (string= ext "org")
                          (format "[%s]({{< relref \"%s\" >}})"
                                  description
                                  export-file-name)
                        (format "![](/ox-hugo/%s)" export-file-name)))
                  (funcall orig-fun link description format)))))

(with-eval-after-load 'embark
  (keymap-binds embark-defun-map
    ("g" . org-dblock-update)))

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
 ("C-c n q c" . citar-create-note)
 ("C-c n q n" . citar-denote-open-note)
 ("C-c n q x" . citar-denote-nocite))

(keymap-binds org-mode-map
  ("C-c n q k" . citar-denote-add-citekey)
  ("C-c n q K" . citar-denote-remove-citekey)
  ("C-c n q d" . citar-denote-dwim)
  ("C-c n q e" . citar-denote-open-reference-entry))

;;; denote-explore
(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-date-identifier-format'.")

(defvar-keymap denote-explore-keymap
  :doc "Denote explore keymap"
  :prefix t
  ;; Statistics
  "c" #'denote-explore-count-notes
  "C" #'denote-explore-count-keywords
  "b" #'denote-explore-barchart-keywords
  "e" #'denote-explore-barchart-filetypes
  ;; Random walks
  "r" #'denote-explore-random-note
  "l" #'denote-explore-random-link
  "k" #'denote-explore-random-keyword
  "x" #'denote-explore-random-regex
  ;; Denote Janitor
  "d" #'denote-explore-identify-duplicate-notes
  "z" #'denote-explore-zero-keywords
  "s" #'denote-explore-single-keywords
  "o" #'denote-explore-sort-keywords
  "w" #'denote-explore-rename-keyword
  ;; Visualise denote
  "n" #'denote-explore-network
  "v" #'denote-explore-network-regenerate
  "D" #'denote-explore-barchart-degree)

(global-bind-keys
 ("C-c n x" . ("Denote Explore" . denote-explore-keymap)))

;;; denote journal
(defun denote-week-report-template ()
  "Generate week template."
  (concat "* 本周工作总结"
          "\n\n"
          "* 下周工作计划"))

;; (setopt denote-journal-signature
;;         (lambda ()
;;           (require 'denote-sequence)
;;           (denote-sequence-get-new 'parent)))

(autoload #'denote-journal-calendar-mode "denote-journal" nil t)
(add-hook 'calendar-mode-hook
          #'denote-journal-calendar-mode)

(with-eval-after-load 'org-capture
  (autoload #'denote-journal-path-to-new-or-existing-entry-filter-report "lib-denote-journal" nil t)
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry
                 (file denote-journal-path-to-new-or-existing-entry-filter-report)
                 "* %U %?\n%i\n%a"
                 :kill-buffer t
                 :empty-lines 1)))

(autoload #'denote-week-report-new-or-existing-entry "lib-denote-journal" nil t)

(defvar-keymap denote-journal-keymap
  :doc "Denote journal keymap"
  :prefix t
  "N" '("New journal" . denote-journal-new-entry)
  "n" '("New or open journal" . denote-journal-new-or-existing-entry)
  "l" '("Link Journal" . denote-journal-link-or-create-entry)
  "w" '("Week report" . denote-week-report-new-or-existing-entry))

(global-bind-keys
 ("C-c n j" . ("Denote Journal" . denote-journal-keymap)))

(provide 'init-writer)
;;; init-writer.el ends here
