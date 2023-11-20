(require 'telega)
;; (require 'telega-url-shorten)

;;; Config
(setq telega-proxies
      `((:server ,user/proxy-host :port ,user/proxy-all-port :enable t
                 :type (:@type "proxyTypeSocks5")))
      telega-chat-show-avatars t
      telega-emoji-use-images nil
      telega-sticker-animated-play t
      telega-auto-translate-probe-language-codes nil
      telega-translate-to-language-by-default "zh-CN"
      telega-chat-input-markups (list "org"))

;; (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

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

(setq telega-avatar-workaround-gaps-for '(return t))

(provide 'init-telega)
