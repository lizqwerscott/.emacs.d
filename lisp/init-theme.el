;;; init-theme.el --- init theme                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "themes"))

(defcustom user/day-theme 'modus-operandi-tinted
  "Day theme name."
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "Night theme name."
  :group 'user
  :type 'symbol)

(require 'consult)
(defun customize-theme (theme &optional previewp)
  "Customize THEME.
PREVIEWP is preview the theme without enabling it permanently."
  (interactive (list (let* ((regexp (consult--regexp-filter
                                     (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                                             consult-themes)))
                            (avail-themes (seq-filter
                                           (lambda (x) (string-match-p regexp (symbol-name x)))
                                           (cons 'default (custom-available-themes))))
                            (saved-theme (car custom-enabled-themes)))
                       (consult--read
                        (mapcar #'symbol-name avail-themes)
                        :prompt "Theme: "
                        :require-match t
                        :category 'theme
                        :history 'consult--theme-history
                        :lookup (lambda (selected &rest _)
                                  (setq selected (and selected (intern-soft selected)))
                                  (or (and selected (car (memq selected avail-themes)))
                                      saved-theme))
                        :state (lambda (action theme)
                                 (with-selected-window (or (active-minibuffer-window)
                                                           (selected-window))
                                   (pcase action
                                     ('return (customize-theme (or theme saved-theme) t))
                                     ((and 'preview (guard theme)) (customize-theme theme t)))))
                        :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (when theme
    (if previewp
        (unless (eq theme (car custom-enabled-themes))
          (mapc #'disable-theme custom-enabled-themes)
          (unless (and (memq theme custom-known-themes) (get theme 'theme-settings))
            (load-theme theme 'no-confirm 'no-enable))
          (if (and (memq theme custom-known-themes) (get theme 'theme-settings))
              (enable-theme theme)
            (consult--minibuffer-message "%s is not a valid theme" theme)))
      (when-let* ((table '(("Night" . user/night-theme)
                           ("Day" . user/day-theme)))
                  (comp (consult--read table
                                       :prompt "Theme Category: "
                                       :require-match t))
                  (symbol (alist-get comp table nil nil #'equal)))
        (+lizqwer/load-theme theme)
        (customize-save-variable symbol theme)))))

(+lizqwer/load-theme user/night-theme)

(provide 'init-theme)
;;; init-theme.el ends here
