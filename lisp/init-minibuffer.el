;;; init-minibuffer.el --- init minibuffer           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; minibuffer

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;;; Vertico
(require 'vertico)

(setq vertico-count 15)

(defun my/vertico-insert ()
  (interactive)
  (let* ((mb (minibuffer-contents-no-properties))
         (lc (if (string= mb "") mb (substring mb -1))))
    (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
          ((file-directory-p (vertico--candidate)) (vertico-insert))
          (t (self-insert-command 1 ?/)))))

;; Configure the display per command.
;; Use a buffer with indices for imenu
;; and a flat (Ido-like) menu for M-x.
(setopt vertico-multiform-commands
        '(;; (consult-imenu buffer indexed)
          ))

;; Configure the display per completion category.
;; Use the grid display for files and a buffer
;; for the consult-grep commands.
(setq vertico-multiform-categories
      '(;; (file grid)
        ;; (project-file grid)
        (consult-grep buffer)))

(defcustom vertico-show-arrow t
  "Vertico arrow."
  :group 'vertico
  :type 'boolean)

(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (let ((arrow (propertize (if (char-displayable-p ?») "» " "> ") 'face 'font-lock-keyword-face)))
    (if (and (not (bound-and-true-p vertico-grid-mode))
             vertico-show-arrow)
        (if (= vertico--index index)
            (concat arrow cand)
          (concat "  " cand))
      cand)))

;; Configure directory extension.
(keymap-binds vertico-map
  ("RET" . vertico-directory-enter)
  ("DEL" . vertico-directory-delete-char)
  ("C-<backspace>" . vertico-directory-delete-word)
  ("C-j" . vertico-exit-input)
  ("/" . my/vertico-insert))

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)

;; Enable vertico-multiform
(vertico-multiform-mode)

;;; marginalia
(marginalia-mode)
(add-hook 'marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode)

;;; consult
(require 'consult)

;; use narrow
(setq consult-narrow-key "<")
;; not auto preview
(setq consult-preview-key "C-o")

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(defun buffer-list-filter ()
  "Get buffer list with filter."
  (let ((buffers (buffer-list))
        (res))
    (dolist (buffer buffers)
      (unless (string-match-p "*help" (buffer-name buffer))
        (push buffer res)))
    res))

(setq consult-buffer-list-function #'buffer-list-filter)

(defun consult-fd-dir ()
  "Consult fd dir."
  (interactive)
  (let ((consult-fd-args (append consult-fd-args
                                 (list
                                  "--type directory"))))
    (consult-fd "~/")))

(defun consult-buffer-with-target (target &optional sources)
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

TARGET is consult buffer target place.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources."
  (interactive)
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt (format "Switch to in other %s: "
                                                  (if target
                                                      (symbol-name target)
                                                    ""))
                                  :history 'consult--buffer-history
                                  :sort nil)))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

(defun consult-buffer-other-window ()
  "Variant of `consult-buffer', switching to a buffer in another window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-buffer-with-target 'window)))

;; for orderless
(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (cdr (orderless-compile input)))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input t str))))

(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

(defun define-minibuffer-key (key &rest defs)
  "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
  (define-key minibuffer-local-map key
              (list 'menu-item nil defs :filter
                    (lambda (d)
                      (plist-get d (completion-metadata-get
                                    (completion-metadata (minibuffer-contents)
                                                         minibuffer-completion-table
                                                         minibuffer-completion-predicate)
                                    'category))))))

(defun consult-find-for-minibuffer ()
  "Search file with find, enter the result in the minibuffer."
  (interactive)
  (pcase-let* ((enable-recursive-minibuffers t)
               (default-directory (file-name-directory (minibuffer-contents)))
               (`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" default-directory))
               (default-directory dir)
               (builder (consult--fd-make-builder paths))
               (file (consult--find prompt builder (file-name-nondirectory (minibuffer-contents)))))
    (delete-minibuffer-contents)
    (insert (expand-file-name file default-directory))
    (exit-minibuffer)))

(define-minibuffer-key "\C-s"
                       'file #'consult-find-for-minibuffer)

;; meow while translate i into TAB
(keymap-unset goto-map "TAB")
(global-unset-key (kbd "C-x C-r"))

(global-bind-keys
 (("C-c s-x" "C-c M-x") . consult-mode-command)

 ("M-y" . consult-yank-pop)

 ([remap Info-search] . consult-info)

 ("C-c b" . consult-buffer)
 ("C-c B" . consult-buffer-other-window)

 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
 ("C-x r b" . consult-bookmark)

 ("M-g l" . consult-goto-line)
 ("M-g o" . consult-outline)
 ("M-g m" . consult-mark)
 ("M-g k" . consult-global-mark)
 ("M-g e" . consult-compile-error)
 ("M-g i" . consult-imenu)
 ("M-g I" . consult-imenu-multi)

 ("M-s l" . consult-line)
 ("M-s L" . consult-line-multi)
 ("M-s u" . consult-isearch-history)
 ("M-s f" . ("Search file" . consult-fd))
 ("M-s d" . ("Search dir" . consult-fd-dir)))

;;; consult dir
(require 'consult-dir)
;; A function that returns a list of directories
(defun consult-dir--quick-dir ()
  "Return list of fasd dirs."
  user/quickdir)

;; A consult source that calls this function
(defvar consult-dir--source-quick
  `(
    :name     "quick"
    :narrow   ?q
    :category file
    :face     consult-file
    :history  file-name-history
    ;; :enabled  t
    :items    ,#'consult-dir--quick-dir)
  "Fasd directory source for `consult-dir'.")

;; Adding to the list of consult-dir sources
(add-to-list 'consult-dir-sources 'consult-dir--source-quick)

(global-bind-keys
 ("C-x C-d" . consult-dir))

;;; consult omni
(with-eval-after-load 'consult-omni
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)
  (setq consult-omni-sources-modules-to-load
        '(consult-omni-duckduckgo consult-omni-calc consult-omni-wikipedia consult-omni-gh))
  (consult-omni-sources-load-modules)

  (setq consult-omni-default-search-engine "DuckDuckGo")
  (setq consult-omni-multi-sources '("calc" "DuckDuckGo API" "Wikipedia" "GitHub")))

(global-bind-keys
 ("M-s w" . ("Search in Web" . consult-omni-multi)))

;;; consult color
(autoload #'consult-colors-web "consult-colors" nil t)
(autoload #'consult-colors-emacs "consult-colors" nil t)

;;; bufferlo

;; modeline
(with-eval-after-load 'bufferlo
  (setq bufferlo-mode-line-prefix "🐃") ; "🐮"
  (setq bufferlo-mode-line-set-active-prefix "Ⓢ")
  (setq bufferlo-mode-line-frame-prefix "Ⓕ")
  (setq bufferlo-mode-line-tab-prefix "Ⓣ")
  (setq bufferlo-mode-line-left-prefix nil)
  (setq bufferlo-mode-line-right-suffix nil))

;; with consult
(defvar my:bufferlo-consult--source-local-buffers
  (list :name "Bufferlo Local Buffers"
        :narrow   ?l
        :category 'buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                              :predicate #'bufferlo-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Local Bufferlo buffer candidate source for `consult-buffer'.")

(defvar my:bufferlo-consult--source-other-buffers
  (list :name "Bufferlo Other Buffers"
        :narrow   ?o
        :category 'buffer
        :hidden   t
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :items    (lambda () (consult--buffer-query
                              :predicate #'bufferlo-non-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

(defvar my:bufferlo-consult--source-all-buffers
  (list :name "Bufferlo All Buffers"
        :narrow   ?a
        :hidden   t
        :category 'buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :items    (lambda () (consult--buffer-query
                              :sort 'visibility
                              :as #'buffer-name)))
  "All Bufferlo buffer candidate source for `consult-buffer'.")

;; add in the reverse order of display preference
;; (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-all-buffers)
(add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
(add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)

(with-eval-after-load 'consult
  (delq 'consult-source-buffer consult-buffer-sources))

;; with tab bar
(setopt tab-bar-new-tab-choice "*scratch*"
        bufferlo-bookmark-tab-replace-policy 'new)

(global-bind-keys
 ("C-c l s" . ("Load save tab" . bufferlo-bookmark-tab-load))
 ("C-c l K" . ("Kill save tab" . bufferlo-bookmark-close-current))

 ("C-c ;" . ("Switch scratch" . bufferlo-switch-to-scratch-buffer)))

(bufferlo-mode)
(bufferlo-anywhere-mode)

;;; embark

;; embark with which-key
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(add-hook 'embark-collect-mode
          #'consult-preview-at-point-mode)

(defun elpaca-browse-1 (item)
  "Get elpaca menu ITEM url."
  (interactive (list (intern (read-from-minibuffer "Input: "))))
  (when-let* ((url (catch 'found
                     (dolist (menu elpaca-menu-functions)
                       (dolist (i (funcall menu 'index))
                         (when (equal item (car i))
                           (throw 'found (plist-get (cdr i) :url))))))))
    (browse-url url)))

(defun my/embark-duckduckgo-search (term)
  "Embark search TERM in duckduckgo."
  (interactive "sSearch Term: ")
  (browse-url
   (format "http://duckduckgo.com/?q=%s" term)))

(defun my/embark-make-directory (dir &optional parents)
  "Make DIR and use `dired' open it.

Interactively, the default choice of directory to create is the
current buffer's default directory.  That is useful when you have
visited a file in a nonexistent directory.

Noninteractively, the second (optional) argument PARENTS, if non-nil, says
whether to create parent directories that don't exist. Interactively, this
happens by default."
  (interactive
   (list (read-file-name "Make directory: " default-directory default-directory
			             nil nil)
	     t))
  (make-directory dir parents)
  (dired dir))

(with-eval-after-load 'embark
  (keymap-binds embark-general-map
    ("G" . my/embark-duckduckgo-search))

  (keymap-binds embark-library-map
    ("b" . elpaca-browse-1)
    ("v" . elpaca-visit))
  (keymap-binds embark-buffer-map
    ("l" . eval-buffer)
    ("B" . switch-to-buffer-other-window))
  (keymap-binds embark-file-map
    ("S" . sudo-edit-find-file)
    ("F" . find-file-other-window)
    ("+" . my/embark-make-directory))
  (keymap-binds embark-bookmark-map
    ("B" . bookmark-jump-other-window)))

(global-bind-keys
 ("s-." . embark-act)
 ("s-;" . embark-dwim)
 ("C-h B" . embark-bindings))

;;; Key-Binding

(global-bind-keys
 (("s-x" "M-x") . execute-extended-command)
 (("s-X" "M-X") . execute-extended-command-for-buffer))

(keymap-binds minibuffer-local-map
  (("s-s" "M-s") . consult-history)
  (("s-r" "M-r") . consult-history)

  (("s-n" "M-n") . next-history-element)
  (("s-p" "M-p") . previous-history-element)

  ("C-i" . (lambda ()
             "Insert the current symbol."
             (interactive)
             (insert (save-excursion
                       (set-buffer (window-buffer (minibuffer-selected-window)))
                       (or (thing-at-point 'symbol t) "")))))
  ("C-x C-d" . consult-dir)
  ("C-x C-j" . consult-dir-jump-file)

  ("C-c C-c" . embark-collect)
  ("C-c C-e" . embark-export))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
