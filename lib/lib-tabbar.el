;;; lib-tabbar.el --- tabbar -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +tab-bar-tab-name-function ()
  "Generate a name for the current tab based on the buffer name.
If the buffer name exceeds `tab-bar-tab-name-truncated-max` characters,
truncate it and append `tab-bar-tab-name-ellipsis`.  If there are multiple
windows in the tab, append the count of windows in parentheses.
Return the formatted tab name."
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

(defun +tab-bar-tab-name-format-function (tab i)
  "Format the display name for a tab in the tab bar.
TAB is the tab descriptor, and I is the tab index.  Apply custom
styling to the tab name and index using `tab-bar-tab-face-function`.

- Prefix the tab with its index and a colon, styled with a bold weight.
- Surround the tab name with spaces, adjusting vertical alignment
  for aesthetics.
- Return the formatted tab name with applied text properties."
  (let ((face (funcall tab-bar-tab-face-function tab)))
    (concat
     ;; change tab-bar's height
     ;; (propertize " " 'display '(raise 0.25))
     (propertize (format "%d:" i) 'face `(:inherit ,face :weight ultra-bold))
     (propertize (concat " " (alist-get 'name tab) " ") 'face face)
     ;; (propertize " " 'display '(raise -0.25))
     )))

(defun tab-create (target-tab-name)
  "Create the NAME tab if it doesn't exist already or switch to that tab.
TARGET-TAB-NAME is the new tab name."
  (let ((current-tab-name (alist-get 'name (tab-bar--current-tab)))
        (tablist (mapcar #'cdadr (tab-bar-tabs))))
    (unless (member target-tab-name tablist)
      (tab-new)
      (tab-bar-rename-tab target-tab-name))
    (tab-bar-select-tab-by-name target-tab-name)))

(defun tab-bar-switch-or-create (tab-name)
  "Create TAB-NAME tabbar or switch it."
  (interactive (list
                (consult--read (mapcar (lambda (tab)
                                         (alist-get 'name tab))
                                       (tab-bar--tabs-recent))
                               :prompt "Input Tab bar name (Default Temp): "
                               :default "Temp"
                               :predicate nil)))
  (let ((tab-index (tab-bar--tab-index-by-name tab-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name))
    tab-index))

(defun tab-bar-switch-or-create-main ()
  "Create or Switch main tab bar."
  (interactive)
  (tab-bar-switch-or-create "Main"))

(defun tab-bar-switch-or-create-rss ()
  "Create or switch elfeed tab bar."
  (interactive)
  (tab-bar-switch-or-create "Rss")
  (call-interactively #'elfeed))

(defun bookmark-jump-other-tab (bookmark)
  "Jump BOOKMARK in other tab.
See `jump-jump' for more."
  (interactive (list (bookmark-completing-read "Jump to bookmark (in other tab)"
                                               bookmark-current-bookmark)))
  (bookmark-jump bookmark 'switch-to-buffer-other-tab))

(provide 'lib-tabbar)
;;; lib-tabbar.el ends here
