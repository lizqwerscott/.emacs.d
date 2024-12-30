;;; init-hydra.el --- init hydra                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

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

(with-eval-after-load 'posframe
  (defun hydra-set-posframe-show-params ()
    "Set hydra-posframe style."
    (setq hydra-posframe-show-params
          `( :left-fringe 8
             :right-fringe 8
             :internal-border-width 2
             :internal-border-color ,(face-background 'posframe-border nil t)
             :background-color ,(face-background 'tooltip nil t)
             :foreground-color ,(face-foreground 'tooltip nil t)
             :lines-truncate t
             :poshandler posframe-poshandler-frame-center-near-bottom)))
  (hydra-set-posframe-show-params)

  (defun start-posframe ()
    (require 'hydra)
    (setq hydra-hint-display-type
          'posframe))

  (defun stop-posframe ()
    (require 'hydra)
    (setq hydra-hint-display-type
          'lv)
    (posframe-delete-all)))

(require 'pretty-hydra)

(custom-set-variables '(pretty-hydra-default-title-body-format-spec " %s%s"))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
     (require 'nerd-icons nil t)))

(cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                    &key face height v-adjust)
  "Add an icon in the hydra title."
  (let ((face (or face `(:inherit highlight :reverse-video t)))
        (height (or height 1.2))
        (v-adjust (or v-adjust 0.0)))
    (concat
     (when (and (icons-displayable-p) icon-type icon-name)
       (let ((f (intern (format "nerd-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
     (propertize title 'face face))))

(defun pretty-hydra-define-add-exit (head-plist)
  (mapcar-if #'(lambda (head)
                 (mapcar-if-not #'(lambda (key)
                                    (append key
                                            (list :exit t)))
                                head
                                #'(lambda (v)
                                    (cl-find :exit v :test #'eq))))
             head-plist
             #'listp))

;;;###autoload
(defmacro pretty-hydra-define-e (name body head-plist)
  (declare (indent 1) (debug (form def-body)))
  `(pretty-hydra-define ,name ,body
     ,(if (cl-getf body :all-exit)
          (pretty-hydra-define-add-exit head-plist)
        head-plist)))

(pretty-hydra-define-e hydra-toggles
  (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on") :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Basic"
   (("p" +lizqwer/toggle-proxy "proxy" :toggle url-proxy-services)
    ("c" global-centered-cursor-mode "centered cursor" :toggle t)
    ("l" interaction-log-mode "interactive log" :toggle t)
    ("i" immersive-translate-auto-mode "immersive translate" :toggle t)
    ("t" +lizqwer/toggle-telega "telega" :toggle (get-buffer "*Telega Root*")))
   "Ui"
   (("n" (display-line-numbers-mode (if display-line-numbers-mode -1 1))
     "line number"
     :toggle (bound-and-true-p display-line-numbers-mode))
    ("d" +lizqwer/toggle-dark-theme "dark theme" :toggle (cl-find user/night-theme custom-enabled-themes))
    ("T" +lizqwer/toggle-transparent "transparent" :toggle (not (eq (frame-parameter (selected-frame) 'alpha-background) 100)))
    ("r" redacted-mode "redacted" :toggle t)
    ("b" imenu-list-smart-toggle "imenu list" :toggle imenu-list-minor-mode)
    ("k" keycast-log-mode "keycast" :toggle t))
   "Edit"
   (("w" toggle-sub-word-or-super-word "sub or super word" :toggle (bound-and-true-p subword-mode))
    ("e" electric-pair-mode "electric pair" :toggle t))
   "Debug"
   (("E" toggle-debug-on-error "debug on error" :toggle (bound-and-true-p debug-on-error)))
   "Program"
   (("f" flymake-mode "flymake" :toggle t)
    ("u" unity-mode "unity develop" :toggle t))))

(pretty-hydra-define-e hydra-jump-dir
  (:title (pretty-hydra-title "Jump to directory" 'octicon "nf-oct-file_directory_open_fill") :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Base"
   (("h" (find-file "~/") "Home")
    ("d" (find-file "~/Downloads") "Downloads")
    ("f" (find-file "~/Documents/") "Documents"))
   "Program"
   (("c" (find-file "~/.emacs.d") "Emacs Config")
    ("g" (find-file "~/github") "Github")
    ("p" (find-file "~/MyProject") "Project"))
   "Search"
   (("s" (lambda ()
           (interactive)
           (autoload 'consult-fd-dir "init-func" nil t)
           (consult-fd-dir)) "Fuzzy search Dir")
    ("j" dired-jump "Dired jump")
    ("J" dired-jump-other-window "Dired jump other"))))

(pretty-hydra-define-e hydra-git
  (:title "Git" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("hunk"
   (("n" diff-hl-next-hunk "Next hunk")
    ("p" diff-hl-previous-hunk "Previous hunk")
    ("s" diff-hl-show-hunk "Show hunk"))
   "git"
   (("b" magit-blame "Blame")
    ("f" magit-find-file "Find git file")
    ("l" magit-log-buffer-file "File log")
    ("h" vc-region-history "History"))))

(pretty-hydra-define-e hydra-language
  (:title "Language" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("dict"
   (("s" sdcv-search-pointer+ "sdcv dict")
    ("f" fanyi-dwim2 "Fanyi Point")
    ("F" fanyi-dwim "Fanyi Input"))
   "english"
   (("t" gt-do-translate "translate")
    ("e" lsp-bridge-toggle-sdcv-helper "english helper"))))

(provide 'init-hydra)
;;; init-hydra.el ends here
