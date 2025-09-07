;;; init-telega.el --- telega                        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'telega-url-shorten-nerd)
(require 'telega-mnz)
(require 'telega-chat)

(when sys/macp
  (setq telega-server-libs-prefix "/opt/homebrew/"))

(setq telega-mnz-languages
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
        (zig . zig-mode)))

;;; Config
(setq telega-chat-show-avatars t
      telega-emoji-use-images nil
      telega-sticker-animated-play t
      telega-auto-translate-probe-language-codes nil
      telega-translate-to-language-by-default "zh-CN"
      telega-chat-input-markups (list "org"))

;; (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

(setq telega-mode-line-mode nil)
(unless sys/macp
  (telega-notifications-mode t))
(global-telega-url-shorten-nerd-mode)

(defun +lizqwer/toggle-telega ()
  "切换telega"
  (interactive)
  (if (get-buffer "*Telega Root*")
      (progn (telega-kill t)
             (message "杀死Telega"))
    (progn (telega t)
           (message "启动Telega"))))

(setq telega-avatar-workaround-gaps-for '(return t))

(defun my/telega-chat-capf ()
  (setq-local completion-at-point-functions
              `(cape-emoji
                ,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev))))
(add-hook 'telega-chat-mode-hook #'my/telega-chat-capf)

(define-key telega-msg-button-map "k" nil)
(define-key telega-msg-button-map "l" nil)
(define-key telega-msg-button-map (kbd "SPC") 'meow-keypad)

(keymap-sets telega-prefix-map
  '(("p" . telega-chatbuf-filter-search)
    ("d" . telega-chat-remove-member)
    ("m" . telega-describe-chat-members)
    ("h" . telega-notifications-history)
    ("x" . telega-chatbuf-thread-cancel)))

(defalias 'telega-prefix-map telega-prefix-map)
(keymap-global-set "C-c t" '("Telega" . telega-prefix-map))

(provide 'init-telega)
;;; init-telega.el ends here
