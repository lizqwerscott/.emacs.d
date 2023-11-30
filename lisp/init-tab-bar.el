(require 'nerd-icons)

;;; tab-bar
(tab-bar-mode t)
(setq tab-bar-new-tab-choice "*scratch*") ;; buffer to show in new tabs
(setq tab-bar-close-button-show nil)      ;; hide tab close / X button
(setq tab-bar-auto-width-max '(300 30))

;; (setq tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

(custom-set-faces
 `(tab-bar                        ((t (:inherit hl-line))))
 `(tab-bar-tab                    ((t (:inverse-video t :bold t))))
 `(tab-bar-tab-inactive           ((t (:inherit shadow))))
 ;; '(tab-bar-tab ((t (:foreground "red"))))
 ;; '(tab-bar-tab-inactive ((t (:foreground "gray"))))
 )

;; (defvar ct/circle-numbers-alist
;;   '((0 . "⓪")
;;     (1 . "①")
;;     (2 . "②")
;;     (3 . "③")
;;     (4 . "④")
;;     (5 . "⑤")
;;     (6 . "⑥")
;;     (7 . "⑦")
;;     (8 . "⑧")
;;     (9 . "⑨"))
;;   "Alist of integers to strings of circled unicode numbers.")

;; (defun ct/tab-bar-tab-name-format-default (tab i)
;;   (let ((current-p (eq (car tab) 'current-tab))
;; 	    (tab-num (if (and tab-bar-tab-hints (< i 10))
;; 		             (alist-get i ct/circle-numbers-alist) "")))
;;     (propertize
;;      (concat tab-num
;; 	         " "
;; 	         (alist-get 'name tab)
;; 	         (or (and tab-bar-close-button-show
;; 			       (not (eq tab-bar-close-button-show
;; 				          (if current-p 'non-selected 'selected)))
;; 			       tab-bar-close-button)
;; 		        "")
;; 	         " ")
;;      'face (funcall tab-bar-tab-face-function tab))))
;; (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)

;;; tab bar telega
(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format " %s "
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face '(:inherit nerd-icons-purple)))
              tab-bar-menu-bar :help "Menu Bar")))

(defun lucius/tab-bar-tab-name-function ()
  (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
         (count (length (window-list-1 nil 'nomini)))
         (truncated-tab-name (if (< (length raw-tab-name)
                                    tab-bar-tab-name-truncated-max)
                                 raw-tab-name
                               (truncate-string-to-width raw-tab-name
                                                         tab-bar-tab-name-truncated-max
                                                         nil nil tab-bar-tab-name-ellipsis))))
    (if (> count 1)
        (concat truncated-tab-name "(" (number-to-string count) ")")
      truncated-tab-name)))

(defun lucius/tab-bar-tab-name-format-function (tab i)
  (let ((face (funcall tab-bar-tab-face-function tab)))
    (concat
     (propertize " " 'face face)
     (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
     (propertize (concat " " (alist-get 'name tab) " ") 'face face))))
;; telega notification
(defvar lucius/tab-bar-telega-indicator-cache nil)

(defun lucius/tab-bar-telega-icon-update (&rest rest)
  (setq lucius/tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                 (telega-server-live-p)
                 (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                                    (telega-filter-chats telega--ordered-chats '(mention)))))
                 ;; 最好使用 (and is-known unread-reactions) temex 来切断一般列表中不可见的聊天
                 ;; 此类聊天，例如对频道中的帖子发表评论，或者您进入、写下一些内容然后离开，然后有人做出反应的聊天
                 (reaction-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count)
                                                   (telega-filter-chats telega--ordered-chats '(and is-known unread-reactions)))))
                 (notification-count (+ mentioned-count unread-count reaction-count)))
            (when (> notification-count 0)
              (concat (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))
                      " "
                      (when (> unread-count 0)
                        (propertize (concat "●" (number-to-string unread-count) " ")
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat "@" (number-to-string mentioned-count) " ")
                                    'face 'telega-mention-count))
                      (when (> reaction-count 0)
                        (propertize (concat "❤" (number-to-string reaction-count) " ")
                                    'face 'telega-mention-count))))))))

(defun lucius/tab-bar-telega-icon ()
  (or lucius/tab-bar-telega-indicator-cache
     (lucius/tab-bar-telega-icon-update)))

(setq tab-bar-tab-name-function 'lucius/tab-bar-tab-name-function
      tab-bar-tab-name-format-function 'lucius/tab-bar-tab-name-format-function
      tab-bar-format '(tab-bar-format-menu-bar
                       lucius/tab-bar-telega-icon
                       tab-bar-format-tabs
                       tab-bar-format-add-tab
                       tab-bar-separator))
(advice-add 'telega--on-updateUnreadChatCount :after #'lucius/tab-bar-telega-icon-update)
(advice-add 'telega--on-updateChatUnreadMentionCount :after #'lucius/tab-bar-telega-icon-update)
(advice-add 'telega--on-updateChatUnreadReactionCount :after #'lucius/tab-bar-telega-icon-update)

(setq tab-bar-tab-hints t)

;;; tabspaces
(tabspaces-mode t)

(setq tabspaces-use-filtered-buffers-as-default t)
(setq tabspaces-default-tab "Default")
(setq tabspaces-remove-to-default t)
(setq tabspaces-initialize-project-with-todo nil)
(setq tabspaces-include-buffers '("*scratch*"))
;; maybe slow
(setq tabspaces-session t)
(setq tabspaces-session-auto-restore nil)

;;; Filter Buffers for Consult-Buffer
(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden nil :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
	      :narrow ?w
	      :history 'buffer-name-history
	      :category 'buffer
	      :state #'consult--buffer-state
	      :default t
	      :items (lambda () (consult--buffer-query
			            :predicate #'tabspaces--local-buffer-p
			            :sort 'visibility
			            :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))


;;;###autoload
(defun tabspaces-open-or-create-workspace (workspace-dir &optional workspace-name)
  (interactive (list (file-truename (read-directory-name "workspace: " "~/"))))
  (let ((existing-tab-names (tabspaces--list-tabspaces))
        (tab-name (if workspace-name
                      workspace-name
                    (car (last (split-string (directory-file-name workspace-dir) "/"))))))
    (cond
     ((member tab-name existing-tab-names)
      (tab-bar-switch-to-tab tab-name))
     (t
      (tab-new)
      (tab-rename tab-name)
      (setq default-directory workspace-dir)))
    (ido-find-file)))


;;;###autoload
(defun tabspaces-open-or-create-telega-workspace ()
  (interactive)
  (let ((existing-tab-names (tabspaces--list-tabspaces))
        (tab-name "telega"))
    (cond
     ((member tab-name existing-tab-names)
      (tab-bar-switch-to-tab tab-name))
     (t
      (tab-new)
      (tab-rename tab-name)))
    (autoload 'telega-switch-important-chat "init-telega" nil t)
    (call-interactively #'telega-switch-important-chat)))

;;;###autoload
(defun tabspaces-open-or-create-temp-workspace ()
  (interactive)
  (let ((existing-tab-names (tabspaces--list-tabspaces))
        (tab-name "temp-code"))
    (cond
     ((member tab-name existing-tab-names)
      (tab-bar-switch-to-tab tab-name))
     (t
      (tab-new)
      (tab-rename tab-name)))
    (ido-find-file-in-dir "~/temp/")))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
