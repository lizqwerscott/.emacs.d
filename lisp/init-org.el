;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
(require 'org)

(require 'lib-org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-link-keep-stored-after-insertion t)

(setq org-enforce-todo-dependencies t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

(setq org-priority-lowest ?D)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(add-hook 'org-mode-hook #'org-cdlatex-mode)

;; Add new template
(add-list-to-list 'org-structure-template-alist
                  '(("n" . "sidenote")
                    ("T" . "typeit")
                    ("!" . "alert")
                    ("L" . "lead")))
;; yank
(with-eval-after-load 'yank-media
  (add-to-list 'yank-media-preferred-types 'image/tiff))

;;; face and font

(defun my/face-with-inherited-foreground (face-name &optional others)
  "Return a face spec for FACE-NAME with bold and inherited foreground.
OTHERS is a plist of additional face attributes to merge.

This function creates a face specification that inherits from FACE-NAME
but adds bold weight and ensures the foreground color is inherited
from FACE-NAME's parent face."
  `(,face-name ((t (:inherit bold :foreground ,(face-foreground face-name nil t) ,@others)))))

(let* ((variable-tuple
        (cond ((x-list-fonts "LXGW WenKai")         '(:font "LXGW WenKai"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))))
  (custom-theme-set-faces
   'user
   (my/face-with-inherited-foreground 'org-level-1 `(,@variable-tuple :height 1.25))
   (my/face-with-inherited-foreground 'org-level-2 `(,@variable-tuple :height 1.1))
   (my/face-with-inherited-foreground 'org-level-3 variable-tuple)
   (my/face-with-inherited-foreground 'org-level-4 variable-tuple)
   (my/face-with-inherited-foreground 'org-level-5 variable-tuple)
   (my/face-with-inherited-foreground 'org-level-6 variable-tuple)
   (my/face-with-inherited-foreground 'org-level-7 variable-tuple)
   (my/face-with-inherited-foreground 'org-level-8 variable-tuple)
   (my/face-with-inherited-foreground 'org-document-title `(,@variable-tuple :height 1.5 :underline nil))))

(custom-theme-set-faces
 'user
 `(variable-pitch ((t (:family "LXGW WenKai" :height ,(round (* 1.2 user/font-size)) :weight thin))))
 `(fixed-pitch ((t ( :family "MonoLisa Lucius" :height ,user/font-size))))
 `(modus-themes-fixed-pitch ((t ( :family "MonoLisa Lucius" :height ,user/font-size)))))

(custom-theme-set-faces
 'user
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8)))))

(add-hook 'org-mode-hook #'variable-pitch-mode)

;;; Org export
(setopt org-export-with-drawers nil
        org-export-with-todo-keywords nil
        org-export-with-toc nil
        org-export-with-smart-quotes t
        org-export-date-timestamp-format "%e %B %Y")
(require 'ox-org)

;; Multiple LaTeX passes for bibliographies
(setopt org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Clean temporary files after export
(setopt org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
                "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
                "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
                "tex" "bcf")))

(with-eval-after-load 'ox-latex
  (add-list-to-list
   'org-latex-classes
   (when-let* ((filename (expand-file-name "config/template/org-latex-class.el"
                                           user-emacs-directory))
               ((file-exists-p filename)))
     (with-temp-buffer
       (insert-file-contents filename)
       (read (current-buffer))))))

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-base-dir
        (file-truename "~/MyProject/website/blog/")
        org-hugo-front-matter-format "yaml"
        org-hugo-auto-set-lastmod t)
  (add-to-list 'org-hugo-special-block-type-properties
               '("sidenote" . (:trim-pre t :trim-post t))))

(with-eval-after-load 'ox
  (require 'ox-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(with-eval-after-load 'ox
  (require 'ox-epub))

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)
   (gnuplot . t)
   (dot . t)
   (shell . t)))

;;; UI

;; Make invisible parts of Org elements appear visible
(add-hook 'org-mode-hook 'org-appear-mode)
;; (add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'org-latex-preview)
(plist-put org-latex-preview-appearance-options
           :page-width 0.8)
(add-hook 'org-mode-hook 'org-latex-preview-mode)

(setq org-latex-preview-mode-display-live t)
(setq org-latex-preview-mode-update-delay 0.25)

;; 中文标记隐藏空格
(unless sys/macp
  (font-lock-add-keywords 'org-mode
                          '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                             (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                            ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                             (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                          'append))

(with-eval-after-load 'org-superstar
  (add-list-to-list 'org-superstar-todo-bullet-alist
                    '(("TODO"   . ?☐)
                      ("DOING"  . ?▶)
                      ("HANGUP" . ?⏸)
                      ("CANCEL" . ?✖))))
(setq org-superstar-special-todo-items t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Make verbatim with highlight text background.
;; (add-to-list 'org-emphasis-alist
;;              '("=" (:background "#fef7ca")))
;; Make deletion(obsolote) text foreground with dark gray.
;; (add-to-list 'org-emphasis-alist
;;            '("+" (:foreground "dark gray"
;;                   :strike-through t)))
;; Make code style around with box.
;; (add-to-list 'org-emphasis-alist
;;              '("~" (:box (:line-width 1
;;                                       :color "grey75"
;;                                       :style released-button))))

(setq valign-facy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

(setopt org-fancy-priorities-list
        '((?A . "⚡")
          (?B . "B")
          (?C . "C")
          (?D . "☕")))
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;;; org rich yank
(defun my-org-rich-yank-format-paste (language contents link)
  "Based on `org-rich-yank--format-paste-default'."
  (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
          language
          (org-rich-yank--trim-nl contents)
          link))
(customize-set-variable 'org-rich-yank-format-paste #'my-org-rich-yank-format-paste)

(require 'org-rich-yank)
(defun org-rich-yank-with-media ()
  "Yank, surrounded by #+BEGIN_SRC block with major mode of originating buffer."
  (interactive)
  (cl-block 'finish
    (when (gui-backend-get-selection 'CLIPBOARD 'image/png)
      (condition-case _
          (progn
            (yank-media)
            (cl-return-from 'finish t))
        (user-error)))
    (let* ((escaped-kill (org-escape-code-in-string (current-kill 0)))
           (needs-initial-newline
            (save-excursion
              (re-search-backward "\\S " (line-beginning-position) 'noerror)))
           (link (org-rich-yank--link))
           (paste (funcall org-rich-yank-format-paste
                           org-rich-yank--lang
                           escaped-kill
                           link)))
      (when needs-initial-newline
        (insert "\n"))
      (insert
       (if org-rich-yank-add-target-indent
           (org-rich-yank-indent paste)
         paste)))))

(keymap-binds org-mode-map
  (("C-s-y" "C-M-y") . org-rich-yank-with-media)
  (("C-s-p" "C-M-p") . org-rich-yank-with-media))

;;; menu
(defun hot-expand (str &optional mod)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert str)
    (if (fboundp 'org-try-structure-completion)
        (org-try-structure-completion) ; < org 9
      (progn
        ;; New template expansion since org 9
        (require 'org-tempo nil t)
        (org-tempo-complete-tag)))
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(with-eval-after-load 'oxr
  ;; oxr insert absolute figure
  (defun oxr-insert-absolute-figure ()
    "Insert a new figure, with name and caption."
    (interactive)
    ;; TODO this inserts absolute paths ATM, which is not ideal.
    (let ((image_file (read-file-name "Image file: ")))
      (insert (oxr--metadata-prompt (oxr--get-name-prefix 'figure) t))
      (org-insert-link 'file image_file))))

(autoload #'oxr-insert-absolute-figure "oxr" nil t)

(require 'lib-transient)
(pretty-transient-define-prefix transient-org-template ()
  "Transient org template menu."
  [["Basic"
    ("e" "example" (hot-expand "<e"))
    ("l" "latex" (hot-expand "<l"))
    ("x" "quote" (hot-expand "<q"))
    ("v" "verse" (hot-expand "<v"))
    ("b" "bash" (hot-expand "<s" "bash"))]
   ["Head"
    ("i" "index" (hot-expand "<i"))
    ("I" "INCLUDE" (hot-expand "<I"))
    ("S" "Startup" (insert "#+STARTUP: "))
    ("H" "Hugo" (yas-expand-snippet (yas-lookup-snippet "hugo")))
    ("L" "LaTeX" (hot-expand "<L"))
    ("X" "Latex chinese" (yas-expand-snippet (yas-lookup-snippet "latex-chinese")))
    ("P" "Latex Preview" (insert "#+STARTUP: latexpreview "))
    ("Mb" "Html Bigblow Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))
    ("Mr" "Html Readtheorg Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))
    ("Mn" "Html Normal Css" (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))]
   ["Source"
    ("ss" "src" (hot-expand "<s"))
    ("se" "emacs-lisp" (hot-expand "<s" "emacs-lisp"))
    ("sp" "python" (hot-expand "<s" "python"))
    ("sP" "python" (hot-expand "<s" "python :results output"))
    ("sc" "c++" (hot-expand "<s" "c++"))
    ("sr" "rust" (hot-expand "<s" "rust"))
    ("sy" "yaml" (hot-expand "<s" "yaml-ts"))]
   ["Oxr"
    ("f" "Figure" oxr-insert-absolute-figure)
    ("t" "Table" oxr-insert-table)
    ("on" "Name" oxr-insert-name)
    ("oe" "Equation" oxr-insert-equation)
    ("os" "Section" oxr-insert-section)]
   ["Hugo"
    ("hn" "sidenote" (hot-expand "<n"))
    ("ht" "typeit" (org-insert-structure-template "typeit"))
    ("ha" "alert" (org-insert-structure-template "alert"))
    ("hl" "lead" (org-insert-structure-template "lead"))]
   ["Misc"
    ("u" "plantuml" (hot-expand "<s" "plantuml :file chart.png"))
    ("G" "gnuplot" (hot-expand "<s" "gnuplot :results output :file ./result.png"))
    ("<" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(defun org-toggle-buffer-link-preview ()
  "Toggle buffer link preview."
  (interactive)
  (if org-link-preview-overlays
      (org-link-preview-clear)
    (org-link-preview-region t)))

(pretty-transient-define-prefix transient-org-toggles ()
  "Transient org menu."
  :transient-non-suffix 'transient--do-stay
  [["Display"
    ("l" "Display Link" org-toggle-link-display :toggle (not org-link-descriptive) :transient t)
    ("m" "Hide Emphasis Markers" org-toggle-display-emphasis-markers :toggle org-hide-emphasis-markers :transient t)
    ("e" "Display Pretty Entities" org-toggle-pretty-entities :toggle org-pretty-entities :transient t)
    ("i" "Buffer Link Preview" org-toggle-buffer-link-preview :toggle org-link-preview-overlays :transient t)
    ("v" "Toggle Valign" valign-mode :toggle t :transient t)]
   ["Org Management"
    ("E" "Export" org-export-dispatch)
    ("L" "List export file" org-list-export-file)]]
  [("q" "Quit" transient-quit-one)])

(defun org-insert-or-surround (open close)
  "Insert or surround text with LaTeX-style delimiters.

If the region is active, wrap the selected text with the delimiters specified by
OPEN and CLOSE. Otherwise, insert the delimiters with space for text in between."
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char begin)
          (insert (format "\\%s " open))
          (goto-char (+ end 3))
          (insert (format " \\%s" close))))
    (insert (format "\\%s  \\%s" open close))
    (backward-char 3)))

(pretty-transient-define-prefix transient-org-line-template ()
  "Transient org line menu."
  [["Link"
    ("r" "Ref" oxr-insert-ref)
    ("l" "Normal" ar/org-insert-link-dwim)
    ("c" "Citre" org-cite-insert)
    ("d" "Denote" denote-insert-link)]
   ["Emphasize"
    ("=" "Verbatim" (org-emphasize ?=))
    ("~" "Code" (org-emphasize ?~))
    ("+" "Delete" (org-emphasize ?+))
    ("_" "Underline" (org-emphasize ?_))

    ("/" "Italic" (org-emphasize ?/))
    ("*" "Bold" (org-emphasize ?*))
    ("e" "Emphasize" org-emphasize)]
   ["Latex"
    ("i" "Inline math" (org-insert-or-surround "(" ")"))
    ("I" "Display math" (org-insert-or-surround "[" "]"))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(transient-define-prefix transient-priority-setting ()
  "Setting priority."
  :transient-non-suffix 'transient--do-stay
  [
   :description (lambda ()
                  (let* ((priority (org-entry-get nil "PRIORITY" t))
                         (priority-char (string-to-char priority)))
                    (format "Priority list: [%s]"
                            (string-join
                             (mapcar (lambda (item)
                                       (pcase-let* ((`(,pro . ,value) item))
                                         (if (equal pro priority-char)
                                             (format "[%s]" value)
                                           value)))
                                     org-fancy-priorities-list)
                             ", "))))
   :class transient-row
   ("K" "↑" org-priority-up :transient t)
   ("J" "↓" org-priority-down :transient t)
   ("q" "Quit" transient-quit-one)])

;;; keymap
(keymap-binds org-mode-map
  ("C-c TAB" . org-insert-item-auto-checkbox)
  ("M-K" . org-metaup)
  ("M-J" . org-metadown)
  ("M-H" . org-metaleft)
  ("M-L" . org-metaright)

  (("M-RET" "s-<return>") . org-meta-return-auto)
  ("s-K" . org-metaup)
  ("s-J" . org-metadown)
  ("s-H" . org-metaleft)
  ("s-L" . org-metaright)

  ("C-c C-l" . ar/org-insert-link-dwim)

  ("M-g o" . consult-org-heading)

  ("C-c o" . transient-org-toggles)
  ("<" . (lambda ()
           "Insert org template."
           (interactive)
           (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
               (transient-org-template)
             (self-insert-command 1))))
  (">" . transient-org-line-template))

(keymap-unset org-mode-map "C-c ;")

(global-bind-keys
 ("C-c c" . org-capture)
 ("C-c A" . org-agenda)
 ("C-c a" . (lambda ()
              "Call Org agenda with `prot-org-custom-daily-agenda' configuration."
              (interactive)
              (org-agenda nil "A")))

 ("C-c L" . org-store-link)
 ("C-c C-o" . org-open-at-point)

 ("M-s n" . consult-notes)
 ("M-s N" . consult-notes-search-in-all-notes))

;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-org-item-map
    ("c" . ("Cycle" . org-cycle-list-bullet)))

  (keymap-binds embark-org-heading-map
    ("n" . ("Note" . org-add-note))
    ("c" . ("Clone" . org-clone-subtree-with-time-shift))
    ("K" . ("Priority Up" . (lambda () (interactive)
                              (org-priority-up)
                              (transient-priority-setting))))
    ("J" . ("Priority Down" . (lambda () (interactive)
                                (org-priority-down)
                                (transient-priority-setting))))))

;;; capf
(defun my/org-capf ()
  "Capf for org mode."
  (setq-local completion-at-point-functions
              `(cape-file
                cape-elisp-block
                pcomplete-completions-at-point)))

(add-hook 'org-mode-hook #'my/org-capf)

;;; another
(setq consult-notes-file-dir-sources
      '(("Org"             ?o "~/Documents/Org/")))
(setq consult-notes-org-headings-files
      '("~/Documents/Org/idea.org"
        "~/Documents/Org/quote.org"))
(consult-notes-org-headings-mode)

(require 'init-org-capture)

(require 'init-org-agenda)

(provide 'init-org)
;;; init-org.el ends here.
