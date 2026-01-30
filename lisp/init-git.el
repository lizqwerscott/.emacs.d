;;; init-git.el --- init package about git           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

(require 'lib-git)

;;; vc
(setq vc-handled-backends '(Git)
      vc-follow-symlinks t)

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(with-eval-after-load 'ediff
  (setq ediff-keep-variants nil
        ediff-make-buffers-readonly-at-startup nil
        ediff-merge-revisions-with-ancestor t
        ediff-show-clashes-only t))

(add-hook 'ediff-startup-hook
          (lambda ()
            (dolist (buffer (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
              (when buffer
                (with-current-buffer buffer
                  (outline-show-all))))))

(casual-ediff-install)
(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu)))

;;; difftastic
(require 'init-difftastic)

;;; diff-mode
(with-eval-after-load 'diff-mode
  (setq diff-default-read-only t
        diff-font-lock-syntax 'hunk-also))

;;; magit
(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (setq magit-delta-hide-plus-minus-markers nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-format-file-function #'magit-format-file-nerd-icons)

  (add-hook 'magit-status-sections-hook
            #'magit-insert-worktrees t))

(with-eval-after-load 'magit-log
  ;; Set `magit-log-margin' value in :init as many other variables will be
  ;; dynamically set based on its value when `magit-log' is loaded.
  ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
  ;; Show the commit ages with 1-char time units
  ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
  ;; Also reduce the author column width to 11 as the author name is being
  ;; abbreviated below.
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
  (advice-add 'magit-log-format-margin :filter-args #'+magit-log--abbreviate-author))

(add-hook 'magit-mode-hook
          #'(lambda ()
              (magit-wip-mode t)
              (magit-delta-mode t)))

(with-eval-after-load 'magit
  (require 'forge))

;;; smerge
(defun transient-smerge-quit-all ()
  "Smerge menu quit."
  (interactive)
  (transient-quit-all)
  (smerge-auto-leave))

(transient-define-prefix transient-smerge-menu ()
  "Smerge conflict resolution menu."
  :transient-non-suffix 'transient--do-stay
  [["Move"
    ("n" "next" smerge-next :transient t)
    ("p" "previous" smerge-prev :transient t)]
   ["Keep"
    ("B" "Base" smerge-keep-base :transient t)
    ("U" "Upper" smerge-keep-upper :transient t)
    ("L" "Lower" smerge-keep-lower :transient t)
    ("A" "All" smerge-keep-all :transient t)
    ("RET" "Current" smerge-keep-current :transient t)]
   ["Diff"
    ("<" "base/upper" smerge-diff-base-upper :transient t)
    ("=" "upper/lower" smerge-diff-upper-lower :transient t)
    (">" "base/lower" smerge-diff-base-lower :transient t)
    ("R" "Refine conflict" smerge-refine :transient t)
    ("E" "Ediff resolve"
     (lambda ()
       (interactive)
       (smerge-ediff)
       (transient-smerge-quit-all)))]
   ["Other"
    ("C" "Combine with next" smerge-combine-with-next :transient t)
    ("r" "Resolve automatically" smerge-resolve :transient t)
    ("K" "Kill current version" smerge-kill-current :transient t)]]
  [[("ZZ" "Save and bury buffer"
     (lambda ()
       (interactive)
       (save-buffer)
       (bury-buffer)
       (transient-smerge-quit-all)))]
   [("q" "cancel" transient-smerge-quit-all)]])

(add-hook 'magit-diff-visit-file-hook
          (lambda ()
            (when smerge-mode
              (transient-smerge-menu))))

;;; menu
(transient-define-prefix git-dispatch ()
  "Git dispatch menu."
  :transient-non-suffix 'transient--do-stay
  [["Hunk"
    ("n" "Next hunk" diff-hl-next-hunk  :transient t)
    ("p" "Previous hunk" diff-hl-previous-hunk :transient t)
    ("r" "Revert hunk" diff-hl-revert-hunk :transient t)
    ("s" "Show hunk" diff-hl-show-hunk)]
   ["Magit"
    ("v" "magit status" unpackaged/magit-status)
    ("d" "magit dispatch" magit-dispatch)
    ("B" "Blame" magit-blame)
    ("f" "Find git file" magit-find-file)]
   ["Log"
    ("oh" "Region history" vc-region-history)
    ("ol" "File log" magit-log-buffer-file)]
   ["Git"
    ("b" "Switch Modified buffer" consult-switch-git-status-buffer)
    ("g" "Git Link" git-link-dispatch)
    ("e" "Ediff revision" casual-ediff-revision)
    ("c" "Clone form clipboard" ar/git-clone-clipboard-url)
    ("R" "Blame reveal mode" blame-reveal-mode)]]
  [("q" "Quit" transient-quit-one)])

(global-bind-keys
 ("C-c v" . git-dispatch))

(provide 'init-git)
;;; init-git.el ends here
