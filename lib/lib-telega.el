;;; lib-telega.el --- telega utils                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'telega)
(require 'nerd-icons)
(require 'cape)

;;; telega notification
(defvar +tab-bar-telega-indicator-cache nil)

(defun +tab-bar-telega-icon-update (&rest _)
  "Update the Telega icon in the tab bar, reflecting notification counts.
This function takes REST as an optional argument, though it is not used
within the function body.

The function checks if the Telega server is live and if the server buffer
is active.  It computes various counts, including:

- The number of unread messages (`unread-count`).
- The number of mentions (`mentioned-count`).
- The number of unread reactions (`reaction-count`).
- The number of keyword matches (`keyword-count`).

The total `notification-count` is the sum of these counts.  If this total
is greater than zero, a formatted string with icons and counts is returned.
This string includes:

- A Telegram icon.
- A bullet with the unread count.
- An at-sign with the mention count.
- A heart with the reaction count.
- A hash with the keyword count.

The function uses `nerd-icons-faicon` for the Telegram icon and applies
specific faces to the counts for visual differentiation."
  (setq +tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* (;; (me-user (telega-user-me 'locally))
                 ;; (online-p (and me-user (telega-user-online-p me-user)))
                 (keyword-count (length (ring-elements telega--notification-messages-ring)))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                                    (telega-filter-chats (telega-chats-list)
                                                      '(mention)))))
                 (notification-count (+ mentioned-count unread-count keyword-count)))
            (when (> notification-count 0)
              (concat (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))
                      (when (> unread-count 0)
                        (propertize (concat " ●​​​" (number-to-string unread-count))
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat " @​​​" (number-to-string mentioned-count))
                                    'face 'telega-mention-count))
                      (when (> keyword-count 0)
                        (propertize (concat " #​​​" (number-to-string keyword-count))
                                    'face 'telega-unmuted-count))))))))

(defun +tab-bar-telega-icon ()
  "Return the Telega icon for the tab bar, updating if necessary.
This function checks if `+tab-bar-telega-indicator-cache` is set.  If it is,
the cached value is returned.  Otherwise, it calls `+tab-bar-telega-icon-update`
to refresh the icon and returns the updated value."
  (or +tab-bar-telega-indicator-cache
      (+tab-bar-telega-icon-update)))

(defun tab-bar-switch-or-create-chat ()
  "Create or switch telega tab bar."
  (interactive)
  (autoload 'tab-bar-switch-or-create "lib-tabbar" nil t)
  (tab-bar-switch-or-create "telega")
  (unless (equal major-mode 'telega-chat-mode)
    ;; create new chat
    (autoload 'telega-chat-with "telega" nil t)
    (call-interactively #'telega-chat-with)))

(defun +lizqwer/toggle-telega ()
  "Toggle telega."
  (interactive)
  (if (get-buffer "*Telega Root*")
      (progn (telega-kill t)
             (message "杀死Telega"))
    (progn (telega t)
           (message "启动Telega"))))

(defun my/telega-chat-capf ()
  "Telega chat capf."
  (setq-local completion-at-point-functions
              `(cape-emoji
                ,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev))))

(provide 'lib-telega)
;;; lib-telega.el ends here
