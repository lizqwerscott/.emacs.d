;;; Highlight uncommitted changes using VC
(require 'diff-hl)
(setq diff-hl-draw-borders nil)

(custom-set-faces
 '(diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
 '(diff-hl-insert ((t (:inherit diff-added :background unspecified))))
 '(diff-hl-delete ((t (:inherit diff-removed :background unspecified)))))

(keymap-set diff-hl-command-map "SPC" 'diff-hl-mark-hunk)

(setq-default fringes-outside-margins t)

(with-no-warnings
  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if sys/linuxp #b11111100 #b11100000))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(global-diff-hl-mode)
(global-diff-hl-show-hunk-mouse-mode)
(add-hook 'dired-mode
          'diff-hl-dired-mode)

(diff-hl-flydiff-mode)

;;; Pulse current line

;; (use-package pulse
;;   :ensure nil
;;   :custom-face
;;   (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
;;   (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
;;   :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
;;          ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
;;   :init
;;   (with-no-warnings
;;     (defun my-pulse-momentary-line (&rest _)
;;       "Pulse the current line."
;;       (pulse-momentary-highlight-one-line (point)))

;;     (defun my-pulse-momentary (&rest _)
;;       "Pulse the region or the current line."
;;       (if (fboundp 'xref-pulse-momentarily)
;;           (xref-pulse-momentarily)
;;         (my-pulse-momentary-line)))

;;     (defun my-recenter-and-pulse(&rest _)
;;       "Recenter and pulse the region or the current line."
;;       (recenter)
;;       (my-pulse-momentary))

;;     (defun my-recenter-and-pulse-line (&rest _)
;;       "Recenter and pulse the current line."
;;       (recenter)
;;       (my-pulse-momentary-line))

;;     (dolist (cmd '(recenter-top-bottom
;;                    other-window switch-to-buffer
;;                    aw-select toggle-window-split
;;                    windmove-do-window-select
;;                    pager-page-down pager-page-up
;;                    treemacs-select-window
;;                    symbol-overlay-basic-jump))
;;       (advice-add cmd :after #'my-pulse-momentary-line))

;;     (dolist (cmd '(pop-to-mark-command
;;                    pop-global-mark
;;                    goto-last-change))
;;       (advice-add cmd :after #'my-recenter-and-pulse))))

;;; Pulse modified region
(add-hooks '(prog-mode text-mode)
           'goggles-mode)

;;; Rainbow
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'rainbow-delimiters)
               (rainbow-delimiters-mode 1)))

(setq color-identifiers:recoloring-delay 1)
(add-hook 'prog-mode-hook
          'color-identifiers-mode)

(add-hook 'web-mode-hook
          #'(lambda ()
              (require 'highlight-matching-tag)
              (highlight-matching-tag 1)))

(require 'paren)
(show-paren-mode 1)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(setq show-paren-style 'parenthesis
      show-paren-context-when-offscreen 'overlay)

;; (use-package highlight-defined
;;   :ensure t
;;   ;; :hook (elisp-lisp-mode . highlight-defined-mode)
;;   )

;;; hl indetn
(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'highlight-indent-guides)
              (setq highlight-indent-guides-auto-odd-face-perc 50)
              (setq highlight-indent-guides-auto-even-face-perc 50)
              (highlight-indent-guides-mode 1)))

;;; hl todo
(require 'init-hl-todo)

(provide 'init-highlight)
