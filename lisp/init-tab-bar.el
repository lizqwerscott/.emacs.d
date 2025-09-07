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
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width t
        ;; Add spaces for tab-name
        tab-bar-tab-name-function '+tab-bar-tab-name-function
        tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function
        tab-bar-format '(tab-bar-format-tabs
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         +tab-bar-telega-icon))

(add-hook 'telega-connection-state-hook #'+tab-bar-telega-icon-update)
(add-hook 'telega-kill-hook #'+tab-bar-telega-icon-update)

(advice-add #'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
(advice-add #'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
(advice-add #'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
(advice-add #'telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)

(tab-rename "Main")

(global-set-keys
 '(("C-c l l" . ("Switch Tab" . tab-bar-switch-to-tab))
   ("C-c l b" . consult-buffer-other-tab)
   ("C-c l n" . ("Switch or Create Tab" . tab-bar-switch-or-create))
   ("C-c l k" . ("Close Tab" . tab-bar-close-tab))
   ("C-c l t" . ("Chat Tab" . tab-bar-switch-or-create-chat))
   ("C-c l m" . ("Main Tab" .  tab-bar-switch-or-create-main))
   ("C-c l r" . ("Rss Tab" . tab-bar-switch-or-create-rss))))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
