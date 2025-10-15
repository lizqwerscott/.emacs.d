;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
(require 'org)

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
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
        \\setstocksize{9.25in}{7.5in}
        \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
        \\setlrmarginsandblock{1.5in}{1in}{*}
        \\setulmarginsandblock{1in}{1.5in}{*}
        \\checkandfixthelayout
        \\layout
        \\setcounter{tocdepth}{0}
        \\renewcommand{\\baselinestretch}{1.25}
        \\setheadfoot{0.5in}{0.75in}
        \\setlength{\\footskip}{0.8in}
        \\chapterstyle{bianchi}
        \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
        \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
        \\setsubsubsecheadstyle{\\normalfont\\centering}
        \\pagestyle{myheadings}
        \\usepackage[font={small, it}]{caption}
        \\usepackage{ccicons}
        \\usepackage{ebgaramond}
        \\usepackage[authoryear]{natbib}
        \\bibliographystyle{apalike}
        \\usepackage{svg}
\\hyphenation{mini-buffer}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

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

;;; Org function
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (expand-file-name "config/template/template.docx" user-emacs-directory)))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file))
    (message "Convert finish: %s" docx-file)))

(defun org-insert-html-head ()
  (interactive)
  (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))

(defun org-insert-html-theme-bigblow ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))

(defun org-insert-html-theme-readtheorg ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))

(defun org-toggle-display-emphasis-markers ()
  "Toggle show emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers
        (not org-hide-emphasis-markers))
  (revert-buffer-quick))

(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

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
(add-hook 'org-mode-hook 'org-fragtog-mode)

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

(setq org-fancy-priorities-list
      '((?A . "A")
        (?B . "⬆")
        (?C . "⬇")
        (?D . "☕")
        (?1 . "⚡")
        (?2 . "2")
        (?3 . "3")
        (?4 . "☕")
        (?I . "Important")))
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;; count word
(with-hook org-mode
  (when (buffer-file-name)
    (org-count-words-mode)))

;;; org rich yank
(defun my-org-rich-yank-format-paste (language contents link)
  "Based on `org-rich-yank--format-paste-default'."
  (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
          language
          (org-rich-yank--trim-nl contents)
          link))
(customize-set-variable 'org-rich-yank-format-paste #'my-org-rich-yank-format-paste)

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

(pretty-hydra-define hydra-org-template
  (:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
          :color blue :quit-key ("q" "C-g"))
  ("Basic"
   (("a" (hot-expand "<a") "ascii")
    ("c" (hot-expand "<c") "center")
    ("C" (hot-expand "<C") "comment")
    ("e" (hot-expand "<e") "example")
    ("E" (hot-expand "<E") "export")
    ("l" (hot-expand "<l") "latex")
    ("x" (hot-expand "<q") "quote")
    ("v" (hot-expand "<v") "verse")
    ("b" (hot-expand "<s" "bash") "bash"))
   "Head"
   (("i" (hot-expand "<i") "index")
    ("A" (hot-expand "<A") "ASCII")
    ("I" (hot-expand "<I") "INCLUDE")
    ("H" (yas-expand-snippet (yas-lookup-snippet "hugo")) "Hugo")
    ("L" (hot-expand "<L") "LaTeX")
    ("S" (insert "#+STARTUP: ") "Startup")
    ("P" (insert "#+STARTUP: latexpreview ") "Latex Preview"))
   "Source"
   (("ss" (hot-expand "<s") "src")
    ("se" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
    ("sp" (hot-expand "<s" "python :results output") "python")
    ("sc" (hot-expand "<s" "c++") "c++")
    ("sr" (hot-expand "<s" "rust") "rust")
    ("sS" (hot-expand "<s" "sh") "sh")
    ("sg" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang")
    ("sx" (hot-expand "<s" "xml") "xml")
    ("sy" (hot-expand "<s" "ymal-ts") "yaml")
    ("sh" (hot-expand "<h") "html"))
   "Oxr"
   (("f" oxr-insert-absolute-figure "Figure")
    ("t" oxr-insert-table "Table")
    ("on" oxr-insert-name "Name")
    ("oe" oxr-insert-equation "Equation")
    ("os" oxr-insert-section "Section"))
   "Hugo"
   (("hn" (hot-expand "<n") "sidenote")
    ("ht" (org-insert-structure-template "typeit") "typeit")
    ("ha" (org-insert-structure-template "alert") "alert")
    ("hl" (org-insert-structure-template "lead") "lead"))
   "Misc"
   (("m" (hot-expand "<s" "mermaid :file chart.png") "mermaid")
    ("u" (hot-expand "<s" "plantuml :file chart.png") "plantuml")
    ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
    ("G" (hot-expand "<s" "gnuplot :results output :file ./result.png") "gnuplot")
    ("<" self-insert-command "ins"))))

(require 'lib-transient)
(pretty-transient-define-prefix transient-org-toggles ()
  "Transient org menu."
  :transient-non-suffix 'transient--do-stay
  [["Display"
    ("l" "Display Link" org-toggle-link-display :toggle (lambda () (not org-link-descriptive)) :transient t)
    ("m" "Hide Emphasis Markers" org-toggle-display-emphasis-markers :toggle (lambda () org-hide-emphasis-markers) :transient t)
    ("e" "Display Pretty Entities" org-toggle-pretty-entities :toggle (lambda () org-pretty-entities) :transient t)
    ("i" "Display inline images" org-toggle-inline-images :toggle (lambda () org-inline-image-overlays) :transient t)
    ("v" "Toggle Valign" valign-mode :toggle t :transient t)]
   ["Org Management"
    ("p" "Set Property" org-set-property)
    ("E" "Export" org-export-dispatch)]]
  [("q" "Quit" transient-quit-one)])

(pretty-transient-define-prefix transient-org-line-template ()
  "Transient org line menu."
  [["Link"
    ("r" "Ref" oxr-insert-ref)
    ("l" "Normal" org-insert-link)
    ("c" "Citre" org-cite-insert)
    ("d" "Denote" denote-insert-link)]
   ["Emphasize"
    ("=" "Verbatim" (lambda ()
                      (interactive)
                      (org-emphasize ?=)))
    ("~" "Code" (lambda ()
                  (interactive)
                  (org-emphasize ?~)))
    ("+" "Delete" (lambda ()
                    (interactive)
                    (org-emphasize ?+)))
    ("_" "Underline" (lambda ()
                       (interactive)
                       (org-emphasize ?_)))

    ("/" "Italic" (lambda ()
                    (interactive)
                    (org-emphasize ?/)))
    ("*" "Bold" (lambda ()
                  (interactive)
                  (org-emphasize ?*)))
    ("e" "Emphasize" org-emphasize)]
   ["Latex"
    ("i" "Inline math" (lambda ()
                         (interactive)
                         (insert "\\(  \\)")
                         (backward-char 3)))
    ("I" "Display math" (lambda ()
                          (interactive)
                          (insert "\\[  \\]")
                          (backward-char 3)))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

;;; keymap
(keymap-binds org-mode-map
  ("C-c TAB" . org-insert-item)
  ("M-K" . org-metaup)
  ("M-J" . org-metadown)
  ("M-H" . org-metaleft)
  ("M-L" . org-metaright)

  ("s-<return>" . org-meta-return)
  ("s-K" . org-metaup)
  ("s-J" . org-metadown)
  ("s-H" . org-metaleft)
  ("s-L" . org-metaright)

  ("C-M-y" . org-rich-yank)
  ("M-g o" . consult-org-heading)

  ("C-c o" . transient-org-toggles)
  ("<" . (lambda ()
           "Insert org template."
           (interactive)
           (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
               (hydra-org-template/body)
             (self-insert-command 1))))
  (">" . transient-org-line-template))

(global-bind-keys
 ("C-c c" . org-capture)
 ("C-c a" . org-agenda)

 ("C-c L" . org-store-link)
 ("C-c C-o" . org-open-at-point)

 ("M-s n" . consult-notes)
 ("M-s N" . consult-notes-search-in-all-notes))

;;; capf
(defun my/org-capf ()
  "Capf for org mode."
  (setq-local completion-at-point-functions
              `(cape-file
                cape-elisp-block
                cape-emoji
                ,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev)
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
