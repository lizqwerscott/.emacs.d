(require 'telega)

(setq telega-proxies
      `((:server ,user/proxy-host :port ,user/proxy-all-port :enable t
                 :type (:@type "proxyTypeSocks5")))
      telega-chat-show-avatars nil
      telega-chat-fill-column 65
      telega-emoji-use-images nil
      telega-auto-translate-probe-language-codes nil
      telega-translate-to-language-by-default "zh-CN"
      telega-chat-input-markups (list "org"))

;; (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

(setq telega-compleleting-read-function 'completing-read)
(setq telega-mode-line-mode nil)
(telega-notifications-mode t)

(defun +lizqwer/toggle-telega ()
  "切换telega"
  (interactive)
  (if (get-buffer "*Telega Root*")
      (progn (telega-kill t)
	         (message "杀死Telega"))
    (progn (telega t)
	       (message "启动Telega"))))

(define-key telega-msg-button-map "k" nil)
(define-key telega-msg-button-map "l" nil)
(define-key telega-msg-button-map (kbd "SPC") 'meow-keypad)
;; (lazy-load-local-keys '(("t" . telega-chat-with))
;; 		              telega-msg-button-map
;; 		              "init-telega")

;; (when (cl-search "arch" operating-system-release)
;;   (use-package telega
;;     :quelpa (telega :fetcher github
;;                     :repo "zevlg/telega.el"
;;                     :branch "master"
;;                     :files (:defaults "contrib" "etc" "server" "Makefile"))
;;     :hook (telega-load . telega-notifications-mode)
;;     :config

;;     ))

(provide 'init-telega)
