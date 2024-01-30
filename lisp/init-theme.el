;; (use-package gruvbox-theme
;;   :ensure t)

;; (use-package doom-themes
;;   :ensure t)

;; (use-package monokai-theme
;;   :ensure t)

;; (use-package solarized-theme
;;   :ensure t
;;   :defer)

;; (use-package modus-themes
;;   :ensure t)

;; (use-package ef-themes
;;   :ensure t
;;   :defer)

;; (use-package flucui-themes
;;   :ensure t
;;   :defer)

;; (load-theme 'dracula t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi t)
;; (load-theme 'doom-snazzy t)
;; (load-theme 'doom-molokai t)
;; (load-theme 'gruvbox-dark-soft t)
;; (load-theme 'doom-one t)
;; (load-theme 'tango-dark t)
;; (load-theme 'monokai t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'vscode-dark-plus t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'ef-summer t)
;; (load-theme 'modus-operandi t)
;; (require 'lazycat-theme)
;; (lazycat-theme-load-dark)
;; (load-theme 'ef-night t)
;; (load-theme 'ef-day t)
;; (load-theme 'ef-summer t)
;; (flucui-themes-load-style 'dark)

;; (use-package circadian
;;   :ensure t
;;   :config
;;   ;; (setq calendar-latitude 37)
;;   ;; (setq calendar-longitude 112)
;;   ;; (setq circadian-themes '((:sunrise . ef-summer)
;;   ;;                          (:sunset  . solarized-dark)))
;;   (setq circadian-themes '(("8:00" . ef-day)
;;                            ("16:00" . solarized-dark)))
;;   (circadian-setup))

;;; Auto change theme, from https://emacs-china.org/t/topic/1348
;; ====================================Themes automatically change =====================================
;;timer for automatically changing themes
(setq mp-ui--interval-timer nil)

;;table is used to save (time themes) pair for automatically changing themes
;;time should be a string. themes should be a variant , not symbos.
(setq mp-ui--time-themes-table nil)

(defun mp-ui/config-time-themes-table (tt)
  "Set time . themes table for time-themes-table."
  (setq mp-ui--time-themes-table
        ;; sort firstly, get-themes-according require a sorted table.
        (sort tt (lambda (x y) (< (string-to-number (car x)) (string-to-number (car y)))))
        )
  )

(defun mp-ui/get-themes-according (hour-string)
  "This function return the theme according to hour-string.
Value of hour-string should be between 1 and 24(including)."
  (catch 'break
    (let (
          (now-time (string-to-number hour-string))
          ;; init current-themes to the themes of final item
          (correct-themes (cdr (car (last mp-ui--time-themes-table))))
          (loop-list mp-ui--time-themes-table)
          )

      ;; loop to set correct themes to correct-themes
      (while loop-list
        (let ((v (car loop-list)))
          (let ((v-time (string-to-number (car v))) (v-themes (cdr v)))
            (if (< now-time v-time)
                (throw 'break correct-themes)  ; t
              (setq correct-themes v-themes) ; nil
              )))
        (setq loop-list (cdr loop-list))
        )
      ;; This is returned for value of hour-string is bigger than or equal to car of final item
      (throw 'break correct-themes) ; t
      ))
  )

(defun mp-ui/check-time-and-modify-theme ()
  "This function will get the theme of now according to time-table-themes,
then check whether emacs should to modify theme, if so, modify it."
  (let ((new-theme (mp-ui/get-themes-according (format-time-string "%H"))))
    (+lizqwer/load-theme new-theme))
  )

(defun mp-ui/open-themes-auto-change ()
  "Start to automatically change themes."
  (interactive)
  (mp-ui/check-time-and-modify-theme)
  (setq
   mp-ui--interval-timer (run-at-time 3600 3600 'mp-ui/check-time-and-modify-theme))
  (message "themes auto change open.")
  )

(defun mp-ui/close-themes-auto-change ()
  "Close automatically change themes."
  (interactive)
  (cancel-timer mp-ui--interval-timer)
  (message "themes auto change close.")
  )

;;Config
(mp-ui/config-time-themes-table `(("6" . ,user/day-theme)
                                  ("17" . ,user/night-theme)))
(mp-ui/open-themes-auto-change)

(provide 'init-theme)
