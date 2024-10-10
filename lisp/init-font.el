(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Source Code Pro" "Jetbrains Mono" "Cascadia Code" "Fira Code"
                           "SF Mono" "Hack" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 220)
                                                      (sys/win32p 110)
                                                      (t 190))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai" "LXGW Neo Xihei" "WenQuanYi Zen Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(setup-fonts)
(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

;;; 连体字体
;; (use-package ligature
;;   :quelpa (ligature :fetcher git :url "https://github.com/mickeynp/ligature.el.git")
;;   :hook
;;   (after-init . global-ligature-mode)
;;   :config
;;   ;; Enable the www ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode
;;                           '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;;                             ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;;                             "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;;                             "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;;                             "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;;                             "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;;                             "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;;                             "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;;                             "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;                             "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

;;; 替换符号
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'pretty-mode)
               (pretty-mode 1)))

(provide 'init-font)
