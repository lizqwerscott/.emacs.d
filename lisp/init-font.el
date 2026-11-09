;;; init-font.el --- font                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst user/default-mac-font-size 230
  "The default font size in mac.")

(defconst user/default-win-font-size 110
  "The default font size in windows.")

(defconst user/default-linux-font-size 190
  "The default font size in linux.")

(defconst user/default-font-list '("MonoLisa Lucius" "Source Code Pro" "Jetbrains Mono" "Cascadia Code" "Fira Code"
                                   "SF Mono" "Hack" "Menlo"
                                   "Monaco" "DejaVu Sans Mono" "Consolas")
  "The default font list.")

(defconst user/default-zh-font-list '("LXGW WenKai Mono" "LXGW WenKai Mono Screen" "LXGW WenKai Screen" "LXGW WenKai Mono" "LXGW Neo XiHei Screen Full" "LXGW WenKai" "LXGW Neo Xihei" "WenQuanYi Zen Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
  "The default zh font list.")

(defconst user/default-mono-font "Maple Mono"
  "The default mono font.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts (&optional font-size)
  "Setup fonts.
FONT-SIZE is the default font size."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in user/default-font-list
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (if font-size
                                                    font-size
                                                  user/font-size)))
    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Symbols Nerd Font Mono" "Segoe UI Symbol" "Symbola" "Symbol")
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


    ;; Force Emacs to search by using font-spec
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append)

    ;; Specify font for Chinese characters
    (cl-loop for font in user/default-zh-font-list
             when (font-installed-p font)
             return (progn
                      (my/font-set-rescale font 1)
                      (set-fontset-font t 'han (font-spec :family font))
                      (my/font-set-rescale font
                                           (/ (* 2.0 (measure-font-pixel-width "h" user/default-mono-font user/font-size))
                                              (measure-font-pixel-width "汉" font user/font-size)))
                      (dolist (charset '(kana han cjk-misc bopomofo hangul))
                        (set-fontset-font t charset
                                          (font-spec :family font)))))))

(defun measure-font-pixel-width (string &optional font-family height)
  "Return the pixel width of STRING rendered in FONT-FAMILY at HEIGHT.

FONT-FAMILY is a family name string such as \"Maple Mono\".
HEIGHT is a face height spec: an integer is an absolute height in
1/10 pt (120 means 12 pt), while a float is a scale factor
relative to the default face (1.5 means 1.5x).

Either argument may be nil to keep the default; when both are nil,
STRING is measured in the default face.

On a TTY frame falls back to character-width based estimation,
independent of FONT-FAMILY and HEIGHT.  If STRING contains
newlines, the width of the widest newline-free line is returned."
  (declare (important-return-value t))
  (if (not (display-graphic-p))
      (let ((max-width 0))
        (dolist (line (split-string string "\n"))
          (setq max-width (max max-width (string-width line))))
        max-width)
    (let* ((face-spec (append (when height     (list :height height))
                              (when font-family (list :family font-family))))
           (proper-string (if face-spec
                              (propertize string 'face face-spec)
                            string)))
      (with-temp-buffer
        (let (deactivate-mark)
          (insert proper-string))
        (add-text-properties
         (point-min) (point-max)
         '(display-line-numbers-disable t line-prefix "" wrap-prefix ""))
        (car (buffer-text-pixel-size nil nil most-positive-fixnum))))))

(defun my/font-set-rescale (font scale)
  "Set FONT SCALE."
  (let ((entry (assoc font face-font-rescale-alist)))
    (if entry
        (setcdr entry scale)
      (push `(,font . ,scale) face-font-rescale-alist))))

(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

(defun set-font-size (symbol value)
  "Set font SYMBOL VALUE."
  (set-default-toplevel-value symbol value)
  (setup-fonts value))

(defcustom user/font-size (cond (sys/macp user/default-mac-font-size)
                                (sys/win32p user/default-win-font-size)
                                (t user/default-linux-font-size))
  "The font size."
  :group 'user
  :type 'number
  :set #'set-font-size)

;;; nerd font
(require 'nerd-icons)
(when (display-graphic-p)
  (unless (find-font (font-spec :name nerd-icons-font-family))
    (nerd-icons-install-fonts t))
  (nerd-icons-set-font))

(defcustom user/ligature nil
  "Is use ligature."
  :group 'user
  :type 'boolean)

;;; 连体字体
(with-eval-after-load 'ligature
  (ligature-set-ligatures 't '("www"))
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

(when user/ligature
  (add-hook 'after-init-hook
            #'global-ligature-mode))

(provide 'init-font)
;;; init-font.el ends here
