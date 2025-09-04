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

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(with-eval-after-load 'ediff
  (setq ediff-keep-variants nil
        ediff-make-buffers-readonly-at-startup nil
        ediff-merge-revisions-with-ancestor t
        ediff-show-clashes-only t))

;;; difftastic
(require 'init-difftastic)

;;; diff-mode
(with-eval-after-load 'diff-mode
  (setq diff-default-read-only t
        diff-font-lock-syntax 'hunk-also))

;;; magit
(require 'init-magit)

;;; smerge
;;; from https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#smerge-mode
(require 'hydra)
(defhydra unpackaged/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _B_ase               _<_: upper/base        _C_ombine
_p_rev       _U_pper              _=_: upper/lower       _r_esolve
^^           _L_ower              _>_: base/lower        _K_ill current
^^           _A_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("B" smerge-keep-base)
  ("U" smerge-keep-upper)
  ("L" smerge-keep-lower)
  ("A" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff :color blue)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("K" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(add-hook 'magit-diff-visit-file-hook
          (lambda ()
            (when smerge-mode
              (unpackaged/smerge-hydra/body))))

;;; menu
(transient-define-prefix git-dispatch ()
  "Git dispatch menu"
  :transient-non-suffix 'transient--do-stay
  [["Hunk"
    ("n" "Next hunk" diff-hl-next-hunk  :transient t)
    ("p" "Previous hunk" diff-hl-previous-hunk :transient t)
    ("r" "Revert hunk" diff-hl-revert-hunk :transient t)
    ("s" "Show hunk" diff-hl-show-hunk)]
   ["Magit"
    ("v" "magit status" unpackaged/magit-status)
    ("d" "magit dispatch" magit-dispatch)
    ("b" "Blame" magit-blame)
    ("f" "Find git file" magit-find-file)]
   ["Log"
    ("oh" "Region history" vc-region-history)
    ("ol" "File log" magit-log-buffer-file)]
   ["Link"
    ("g" "Git Link" git-link-dispatch)]]
  [("q" "Quit" transient-quit-one)])

(global-set-keys
 '(("C-c v" . git-dispatch)))

(provide 'init-git)
;;; init-git.el ends here
