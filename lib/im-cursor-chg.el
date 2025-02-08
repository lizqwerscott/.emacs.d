;;; im-cursor-chg.el --- Change cursor color for input method  -*- lexical-binding: t; -*-

;; Inspired by code from cursor-chg
;; URL: https://github.com/emacsmirror/cursor-chg/blob/master/cursor-chg.el

;;; Commentary:
;;
;; To turn on the cursor color change by default,
;; put the following in your Emacs init file.
;;
;; (require 'im-cursor-chg)
;; (cursor-chg-mode 1)
;;
;;; Code:

(require 'rime nil t)

(defvar im-cursor-color "Orange"
  "The color for input method.")

(defvar im-default-cursor-color (frame-parameter nil 'cursor-color)
  "The default cursor color.")

(defun im--chinese-p ()
  "Check if the current input state is Chinese."
  (if (featurep 'rime)
      (and (rime--should-enable-p)
           (not (rime--should-inline-ascii-p))
           current-input-method)
    current-input-method))

(defun im-change-cursor-color ()
  "Set cursor color depending on input method."
  (interactive)
  (set-cursor-color (if (im--chinese-p)
                        im-cursor-color
                      im-default-cursor-color)))

(define-minor-mode cursor-chg-mode
  "Toggle changing cursor color.
With numeric ARG, turn cursor changing on if ARG is positive.
When this mode is on, `im-change-cursor-color' control cursor changing."
  :init-value nil :global t :group 'frames
  (if cursor-chg-mode
      (add-hook 'post-command-hook 'im-change-cursor-color)
    (remove-hook 'post-command-hook 'im-change-cursor-color)))


(provide 'im-cursor-chg)
;;; im-cursor-chg.el ends here
