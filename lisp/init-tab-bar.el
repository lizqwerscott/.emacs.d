;;; init-tab-bar.el --- init tab bar                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'tab-bar)
(require 'lib-tabbar)
(setq tab-bar-separator " ")
(setopt tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-to 'rightmost
        tab-bar-tab-hints t
        tab-bar-show 1
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width t
        ;; Add spaces for tab-name
        tab-bar-tab-name-function '+tab-bar-tab-name-function
        tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function
        tab-bar-format '(tab-bar-format-tabs
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         +tab-bar-telega-icon))

(tab-rename "Main")

(global-bind-keys
 ("C-c l l" . ("Switch Tab" . tab-bar-switch-to-tab))
 ("C-c l b" . consult-buffer-other-tab)
 ("C-c l f" . find-file-other-tab)
 ("C-c l B" . bookmark-jump-other-tab)
 ("C-c l n" . ("Switch or Create Tab" . tab-bar-switch-or-create))
 ("C-c l k" . ("Close Tab" . tab-bar-close-tab))
 ("C-c l m" . ("Main Tab" .  tab-bar-switch-or-create-main)))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
