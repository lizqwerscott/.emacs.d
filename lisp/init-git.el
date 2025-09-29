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

(defun consult-switch-git-status-buffer ()
  "Parse git status from an expanded path and switch to a file.
The completion candidates include the Git status of each file."
  (interactive)
  (require 'vc-git)
  (let ((repo-root (vc-git-root default-directory)))
	(if (not repo-root)
		(message "Not inside a Git repository.")
	  (let* ((expanded-root (expand-file-name repo-root))
			 (command-to-run (format "git -C %s status --porcelain=v1"
									 (shell-quote-argument expanded-root)))
			 (cmd-output (shell-command-to-string command-to-run))
			 (target-files
			  (let ((staged-files)
                    (unstaged-files)
                    (untracked-files))
				(dolist (line (split-string cmd-output "\n" t) ;; (nreverse files)
                              `(("staged" . ,staged-files)
                                ("unstaged" . ,unstaged-files)
                                ("untracked" . ,untracked-files)))
				  (when (> (length line) 3)
					(let ((status (substring line 0 2))
						  (path-info (substring line 3)))
					  ;; Handle rename specially
					  (if (string-match "^R" status)
						  (let* ((paths (split-string path-info " -> " t))
								 (new-path (cadr paths)))
							(when new-path
							  (push (cons (format "R %s" new-path) new-path) files)))
						;; Modified or untracked
                        (cond
                         ((string-prefix-p "M" status)
                          (push path-info staged-files))
                         ((string-prefix-p " M" status)
                          (push path-info unstaged-files))
                         ((string-match "\?\?" status)
                          (push path-info untracked-files))))))))))
		(if (not target-files)
			(message "No modified or renamed files found.")
		  (let* ((candidates (cl-loop for (gname . items) in target-files
                                      append (mapcar (lambda (it) (cons it gname)) items)))
		    	 (selection (consult--read (mapcar #'car candidates)
                                           :prompt "Switch to buffer (Git modified): "
                                           :predicate nil
                                           :require-match t
                                           :group (lambda (cand transform)
                                                    (if transform
                                                        (concat (nerd-icons-icon-for-file cand)
                                                                " "
                                                                cand)
                                                      (alist-get cand candidates nil nil #'equal)))
                                           :sort nil)))
		    (when selection
		      (let ((file-path selection))
		    	(when file-path
		    	  (find-file (expand-file-name file-path expanded-root)))))))))))

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
    ("B" "Blame" magit-blame)
    ("f" "Find git file" magit-find-file)]
   ["Log"
    ("oh" "Region history" vc-region-history)
    ("ol" "File log" magit-log-buffer-file)]
   ["Git"
    ("b" "Switch Modified buffer" consult-switch-git-status-buffer)
    ("g" "Git Link" git-link-dispatch)]]
  [("q" "Quit" transient-quit-one)])

(global-set-keys
 '(("C-c v" . git-dispatch)))

(provide 'init-git)
;;; init-git.el ends here
