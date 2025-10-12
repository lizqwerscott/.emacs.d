;;; init-highlight.el --- init highlight             -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Highlight the current line
(global-hl-line-mode 1)
(add-hooks '(dashboard-mode eshell-mode shell-mode term-mode vterm-mode)
           #'(lambda () (setq-local global-hl-line-mode nil)))

;;; Highlight matching parens
(require 'paren)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery nil
      show-paren-delay 0.2)
(setq show-paren-style 'parenthesis
      show-paren-context-when-offscreen 'overlay)
(custom-set-faces
 '(show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold)))))
(show-paren-mode 1)

;;; highlight parentheses
(require 'highlight-parentheses)
(setq highlight-parentheses-colors '("firebrick1" "IndianRed1" "orange1" "orange3")
      highlight-parentheses-attributes '((:underline t :weight bold))
      highlight-parentheses-delay 0.2)

(add-hook 'minibuffer-setup-hook
          #'highlight-parentheses-minibuffer-setup)
(add-hook 'prog-mode-hook
          #'highlight-parentheses-mode)

;;; Highlight symbol
(setq symbol-overlay-idle-time 0.1)

(add-hooks '(prog-mode yaml-ts-mode)
           #'symbol-overlay-mode)

;;; Highlight indentions
(setopt indent-bars-color '(highlight :face-bg t :blend 0.225)
        indent-bars-pattern "."
        indent-bars-width-frac 1
        indent-bars-pad-frac 0.1
        indent-bars-zigzag nil
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
        indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
        indent-bars-display-on-blank-lines t
        indent-bars-prefer-character nil

        indent-bars-treesit-support t
        indent-bars-no-descend-string t
        indent-bars-treesit-ignore-blank-lines-types '("module")
        indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement)))

(with-eval-after-load 'indent-bars
  (require 'indent-bars-ts))

(add-hooks '(python-mode python-ts-mode rust-mode rust-ts-mode c++-mode c++-ts-mode)
           #'indent-bars-mode)

;;; Colorize color names in buffers
(setq colorful-use-prefix t)
(add-hook 'prog-mode-hook
          #'colorful-mode)

;;; Highlight brackets according to their depth
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'rainbow-delimiters)
               (rainbow-delimiters-mode 1)))

;;; Highlight todo
(require 'init-hl-todo)

;;; Highlight uncommitted changes using VC
(require 'diff-hl)
(setq diff-hl-draw-borders nil)
(setq diff-hl-disable-on-remote t)

(custom-set-faces
 '(diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
 '(diff-hl-insert ((t (:inherit diff-added :background unspecified))))
 '(diff-hl-delete ((t (:inherit diff-removed :background unspecified)))))

(keymap-set diff-hl-command-map "SPC" 'diff-hl-mark-hunk)
(keymap-unset diff-hl-command-map "n")

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
(add-hook 'dired-mode-hook
          'diff-hl-dired-mode)

(diff-hl-flydiff-mode)

;;; Pulse modified region
(require 'init-pulsar)

;;; Highlight web mode matching tag
(add-hook 'web-mode-hook
          #'(lambda ()
              (require 'highlight-matching-tag)
              (highlight-matching-tag 1)))

;;; Highlight lisp
(add-hooks '(lisp-mode emacs-lisp-mode)
           #'(lambda ()
               (highlight-function-calls-mode 1)
               (lisp-extra-font-lock-mode 1)))

(provide 'init-highlight)
;;; init-highlight.el ends here
