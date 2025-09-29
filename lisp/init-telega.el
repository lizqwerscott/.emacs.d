;;; init-telega.el --- telega                        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages!
 '((telega :fetcher github
           :repo "zevlg/telega.el"
           :branch "master")
   (telega-url-shorten-nerd :fetcher github
                            :repo "lizqwerscott/telega-url-shorten-nerd")))

(with-eval-after-load 'telega-mnz
  (setopt telega-mnz-languages
          '((ada . ada-mode)
            (awk . awk-mode)
            (c . c-ts-mode)
            (clojure . clojure-mode)
            (cpp . c++-ts-mode)
            (csharp . csharp-ts-mode)
            (scheme . scheme-mode)              ; not in language-detection
            (css . css-mode)
            (dart . dart-mode)
            (delphi . delphi-mode)
            (diff . diff-mode)                  ; not in language-detection
            (emacslisp . emacs-lisp-mode)
            (erlang . erlang-mode)
            (fortran . fortran-mode)
            (go . go-ts-mode)
            (groovy . groovy-mode)
            (haskell . haskell-mode)
            (html . html-mode)
            (java . java-mode)
            (javascript . javascript-mode)
            (json . json-mode)
            (kotlin . kotlin-mode)              ; not in language-detection
            (latex . latex-mode)
            (lisp . lisp-mode)
            (lua . lua-mode)
            (matlab . matlab-mode)
            (objc . objc-mode)
            (octave . octave-mode)              ; not in language-detection
            (org . org-mode)                    ; not in language-detection
            (outline . outline-mode)            ; not in language-detection
            (perl . perl-mode)
            (php . php-mode)
            (prolog . prolog-mode)
            (python . python-ts-mode)
            (r . ess-r-mode)
            (ruby . ruby-mode)
            (rust . rust-ts-mode)
            (scala . scala-mode)
            (shell . bash-ts-mode)
            (smalltalk . smalltalk-mode)
            (sml . sml-mode)
            (sql . sql-mode)
            (swift . swift-mode)
            (visualbasic . visual-basic-mode)
            (xml . xml-mode)
            (zig . zig-mode))))


(with-eval-after-load 'telega
  (require 'telega-url-shorten-nerd)
  (require 'telega-mnz)
  (require 'telega-chat)
  (require 'lib-telega)

  (setopt telega-chat-show-avatars t
          telega-emoji-use-images nil
          telega-sticker-animated-play t
          telega-auto-translate-probe-language-codes nil
          telega-translate-to-language-by-default "zh-CN"
          telega-chat-input-markups (list "org")
          telega-avatar-workaround-gaps-for '(return t))

  ;; (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

  (when sys/macp
    (setq telega-server-libs-prefix "/opt/homebrew/"))

  ;;; keymap
  (keymap-binds telega-msg-button-map
    ("k" . nil)
    ("l" . nil)
    ("SPC" . meow-keypad))

  (keymap-binds telega-prefix-map
    ("p" . telega-chatbuf-filter-search)
    ("d" . telega-chat-remove-member)
    ("m" . telega-describe-chat-members)
    ("h" . telega-notifications-history)
    ("x" . telega-chatbuf-thread-cancel))

  (defalias 'telega-prefix-map telega-prefix-map)

  (global-bind-keys
   ("C-c t" . ("Telega" . telega-prefix-map))

   ("C-c l t" . ("Chat Tab" . tab-bar-switch-or-create-chat)))

  ;;; notification
  (add-hook 'telega-connection-state-hook #'+tab-bar-telega-icon-update)
  (add-hook 'telega-kill-hook #'+tab-bar-telega-icon-update)

  (advice-add #'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)

  (add-hook 'telega-chat-mode-hook #'my/telega-chat-capf)
  (unless sys/macp
    (telega-notifications-mode t))

  (global-telega-url-shorten-nerd-mode))

(when (and user/telega-start (display-graphic-p))
  (add-hook 'after-init-hook
            (lambda ()
              (message "start telega")
              (autoload '+lizqwer/toggle-telega "lib-telega" nil t)
              (+lizqwer/toggle-telega))))

(provide 'init-telega)
;;; init-telega.el ends here
