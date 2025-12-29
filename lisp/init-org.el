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

;;; Org function
(defun org-export-docx (&optional template-select)
  "Use pandoc convert org to Docx.

if TEMPLATE-SELECT is not nil, select a template file."
  (interactive "P")
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (file-truename
                        (if template-select
                            (read-file-name "Select Template file: "
                                            (expand-file-name "config/template/"
                                                              user-emacs-directory))
                          (expand-file-name "config/template/template.docx" user-emacs-directory)))))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file)
     (get-buffer-create "*Org export docx*")
     (get-buffer-create "*Org export docx error*"))
    (message "Convert finish: %s" docx-file)))

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

(defun org-insert-item-auto-checkbox ()
  "Org insert auto-checkbox item."
  (interactive)
  (org-insert-item
   (and (org-in-item-p)
        (save-excursion
          (looking-back "\\].**" (line-beginning-position))))))

(defun org-meta-return-auto (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item-auto-checkbox' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				                ((org-at-table-p) #'org-table-wrap-region)
				                ((org-in-item-p) #'org-insert-item-auto-checkbox)
				                (t #'org-insert-heading)))))

(defun org-get-export-file-paths ()
  "Get all possible export file paths for current Org buffer.
Returns an alist where keys are export formats and values are file paths."
  (interactive)
  (let ((paths '())
        (formats '(("html" . ".html")
                   ("latex" . ".tex")
                   ("pdf" . ".pdf")
                   ("markdown" . ".md")
                   ("odt" . ".odt")
                   ("org" . ".org")
                   ("ascii" . ".txt")
                   ("texinfo" . ".texi"))))
    (dolist (format formats)
      (let* ((backend (car format))
             (extension (cdr format))
             (output-file (org-export-output-file-name extension)))
        (when (file-exists-p output-file)
          (push (cons backend output-file) paths))))
    paths))

(defun org-list-export-file ()
  "Open export files dired buffer."
  (interactive)
  (let* ((export-files (org-get-export-file-paths))
         (files (mapcar #'cdr export-files)))
    (cond
     ((null files)
      (message "No export files found for current buffer"))
     (t
      (dired (cons default-directory files))))))

(defun my/org-open-links-in-dired ()
  "Open all ‘file:’ links in Dired buffer."
  (interactive)
  (let* ((links
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (when (string= (org-element-property :type link) "file")
                (org-element-property :path link)))))
         (abs-paths
          (mapcar (lambda (path)
                    (expand-file-name path default-directory))
                  links)))
    (if abs-paths
        (dired abs-paths)
      (message "No file links found in this buffer."))))

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

(advice-add #'org-count-words-update-buffer-count
            :before (lambda (_ &rest _)
                      (when-let* ((name (buffer-name))
                                  ((or (string-match-p "\\*Capture\\*" name)
                                       (string-match-p "^CAPTURE-.*" name))))
                        (org-count-words-mode -1))))

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
    ("S" (insert "#+STARTUP: ") "Startup")

    ("H" (yas-expand-snippet (yas-lookup-snippet "hugo")) "Hugo")

    ("L" (hot-expand "<L") "LaTeX")
    ("X" (yas-expand-snippet (yas-lookup-snippet "latex-chinese")) "Latex chinese")
    ("P" (insert "#+STARTUP: latexpreview ") "Latex Preview")

    ("Mb" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup") "Html Bigblow Theme")
    ("Mr" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup") "Html Readtheorg Theme")
    ("Mn" (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>") "Html Normal Css"))
   "Source"
   (("ss" (hot-expand "<s") "src")
    ("se" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
    ("sp" (hot-expand "<s" "python") "python")
    ("sP" (hot-expand "<s" "python :results output") "python")
    ("sc" (hot-expand "<s" "c++") "c++")
    ("sr" (hot-expand "<s" "rust") "rust")
    ("sS" (hot-expand "<s" "sh") "sh")
    ("sg" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang")
    ("sx" (hot-expand "<s" "xml") "xml")
    ("sy" (hot-expand "<s" "yaml-ts") "yaml")
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
                         (org-insert-or-surround "(" ")")))
    ("I" "Display math" (lambda ()
                          (interactive)
                          (org-insert-or-surround "[" "]")))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

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

  ("M-g n" . org-next-visible-heading)
  ("M-g p" . org-previous-visible-heading)

  ("M-g o" . consult-org-heading)

  ("C-c o" . transient-org-toggles)
  ("<" . (lambda ()
           "Insert org template."
           (interactive)
           (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
               (hydra-org-template/body)
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
