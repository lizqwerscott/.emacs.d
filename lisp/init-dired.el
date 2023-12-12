(setq dired-recursive-deletes 'always)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

(require 'dired)
(require 'dired-x)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*$"))

(add-hook 'dired-mode-hook
          #'(lambda ()
              (nerd-icons-dired-mode)
              (diredfl-mode)
              (dired-omit-mode)))

(require 'dired-subtree)
(keymap-sets dired-mode-map
             '(("TAB" . dired-subtree-cycle)
               ("f" . consult-fd-dir)))

;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-mode-line-format
;;    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   ;; (dirvish-attributes '(subtree-state all-the-icons vc-state git-msg))
;;   (dirvish-attributes '(all-the-icons vc-state file-size))
;;   (dirvish-preview-dispatchers '(vc-diff))
;;   (dirvish-mode-line-height 0)
;;   (dirvish-header-line-height 0)
;;   :bind
;;   (:map dirvish-mode-map
;;         ("/" . dirvish-fd)
;;         ("a"   . dirvish-quick-access)
;;         ("f"   . dirvish-file-info-menu)
;;         ("y"   . dirvish-yank-menu)
;;         ("N"   . dirvish-narrow)
;;         ("^"   . dirvish-history-last)
;;         ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;         ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;         ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;         ("TAB" . dirvish-subtree-toggle)
;;         ("h" . dired-up-directory)
;;         ("q" . my/meow-quit))
;;   :config
;;   (dirvish-peek-mode))

(provide 'init-dired)
