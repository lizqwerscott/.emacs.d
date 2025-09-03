;;; init-minibuffer.el --- init minibuffer           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; minibuffer

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;;; Vertico
(require 'vertico)

(setq vertico-count 15)

;; Configure directory extension.
(keymap-sets vertico-map
  '(("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    (("M-DEL" "s-DEL") . vertico-directory-up)
    ("s-RET" . vertico-exit-input)
    ("C-j" . vertico-exit-input)))

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)

;;; marginalia
(marginalia-mode)
(add-hook 'marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode)

;;; consult
(require 'consult)

;; use narrow
(setq consult-narrow-key "<")
;; not auto preview
(setq consult-preview-key "C-o")

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(defun buffer-list-filter ()
  "Get buffer list with filter."
  (let ((buffers (buffer-list))
        (res))
    (dolist (buffer buffers)
      (unless (string-match-p "*help" (buffer-name buffer))
        (push buffer res)))
    res))

(setq consult-buffer-list-function #'buffer-list-filter)

;; consult dir
(require 'consult-dir)
;; A function that returns a list of directories
(defun consult-dir--quick-dir ()
  "Return list of fasd dirs."
  (list "~" "~/Downloads/" "~/Documents/" "~/MyProject/" "~/github/"))

;; A consult source that calls this function
(defvar consult-dir--source-quick
  `(
    :name     "quick"
    :narrow   ?q
    :category file
    :face     consult-file
    :history  file-name-history
    ;; :enabled  t
    :items    ,#'consult-dir--quick-dir)
  "Fasd directory source for `consult-dir'.")

;; Adding to the list of consult-dir sources
(add-to-list 'consult-dir-sources 'consult-dir--source-quick)

(global-set-keys
 '(("C-x C-d" . consult-dir)))

;;; embark
(add-hook 'embark-collect-mode
          #'consult-preview-at-point-mode)

(global-set-keys
 '(("s-." . embark-act)
   ("s-;" . embark-dwim)
   ("C-h B" . embark-bindings)))


(keymap-sets minibuffer-local-map
  '(("M-s" . consult-history)
    ("M-r" . consult-history)
    ("C-i" . (lambda ()
               "Insert the currunt symbol."
               (interactive)
               (insert (save-excursion
                         (set-buffer (window-buffer (minibuffer-selected-window)))
                         (or (thing-at-point 'symbol t) "")))))
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)

    ("C-c C-c" . embark-collect)
    ("C-c C-e" . embark-export)))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
