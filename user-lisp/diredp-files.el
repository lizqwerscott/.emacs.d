;;; diredp-files.el --- Dired files plus             -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; from https://www.emacswiki.org/emacs/DiredPlus

;;; Code:

(require 'dired)
(require 'dired-aux)


(defcustom diredp-quote-copied-filenames-flag t
  "Non-nil means \\<dired-mode-map>\\[dired-copy-filename-as-kill] double-quotes \
file names containing SPC, ', or \"."
  :group 'Dired-Plus :type 'boolean)

(defcustom diredp-list-file-attributes (list 5 8)
  "Which file attributes `diredp-list-file' uses, and when.
A list of file attribute numbers means use only the values of those
attributes.
A non-list means use all attribute values."
  :group 'Dired-Plus :type '(choice
                             (repeat (integer :tag "Use attribute with number"))
                             (const :tag "Use all attributes" all)))

(defvar diredp-last-copied-filenames ()
  "String holding the file names copied by `dired-copy-filename-as-kill'.")
;; NOTE: `diredp-yank-files' and `diredp-move-files-named-in-kill-ring' read THIS
;; variable, not the head of the kill ring, so pasting is unaffected by later kills.

(defvar diredp-filename-separator (copy-sequence "\000") ; ^@ (NUL)
  "String used to separate file names in a `kill-ring' list of file names.
A NUL byte cannot occur in a file name, so it is safe even when file names
contain spaces.  (This is the same trick the full Dired+ uses.)")

(defun diredp-ensure-mode ()
  "Raise an error unless in Dired or a mode derived from it."
  (unless (derived-mode-p 'dired-mode)
    (error "You must be in Dired or a mode derived from it to use this command")))

(defun diredp-delete-if-not (predicate xs)
  "Remove from list XS all elements that do not satisfy PREDICATE.
Destructive; reuses XS's conses where possible."
  (while (and xs  (not (funcall predicate (car xs))))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (not (funcall predicate (cadr cl-p)))
          (setcdr cl-p (cddr cl-p))
        (setq cl-p  (cdr cl-p)))))
  xs)

(defun diredp-list-file (file &optional details)
  "Return FILE name, expanded.
Non-nil optional arg DETAILS means append details about FILE to the
returned string.

If DETAILS is a list of file attribute numbers then include only the
values of those attributes.  Otherwise, include all attribute values."
  (let ((file-dir  (and details  (or (file-name-directory file)  default-directory)))
        attrs)
    (setq file  (expand-file-name file file-dir))
    (when (and details  (atom details)) (setq details  '(0 1 2 3 4 5 6 7 8 9 10 11)))
    (concat
     file
     (and details
          (setq attrs  (file-attributes file))
          (concat
           "\n"
           (and (memq 0 details)
                (format " File type:                  %s\n"
                        (cond ((eq t (nth 0 attrs))  "Directory")
                              ((stringp (nth 0 attrs))  (format "Symbolic link to `%s'" (nth 0 attrs)))
                              (t  "Normal file"))))
           (and (memq 8 details)
                (format " Permissions:                %s\n" (nth 8 attrs)))
           (and (memq 7 details)  (not (eq t (nth 0 attrs)))
                (format " Size in bytes:              %g\n" (nth 7 attrs)))
           (and (memq 4 details)
                (format-time-string " Time of last access:        %a %b %e %T %Y (%Z)\n" (nth 4 attrs)))
           (and (memq 5 details)
                (format-time-string " Time of last modification:  %a %b %e %T %Y (%Z)\n" (nth 5 attrs)))
           (and (memq 6 details)
                (format-time-string " Time of last status change: %a %b %e %T %Y (%Z)\n" (nth 6 attrs)))
           (and (memq 1 details)
                (format " Number of links:            %d\n" (nth 1 attrs)))
           (and (memq 2 details)
                (format " User ID (UID):              %s\n" (nth 2 attrs)))
           (and (memq 3 details)
                (format " Group ID (GID):             %s\n" (nth 3 attrs)))
           (and (memq 10 details)
                (format " Inode:                      %S\n" (nth 10 attrs)))
           (and (memq 11 details)
                (format " Device number:              %s\n" (nth 11 attrs))))))))

(defun diredp-list-files (files &optional dir bufname predicate details)
  "Display FILES, a list of file names.  Wildcard patterns are expanded.
The files are shown in a new buffer, `*Files*' by default.

Optional arg DIR serves as the default directory for expanding file
 names that are not absolute.  It defaults to `default-directory'.

Optional arg BUFNAME is the name of the buffer for the display.
 It defaults to `*Files*' (or `*Files*<N>' if `*Files*' exists).

Optional arg PREDICATE is a predicate used to filter FILES: only files
 satisfying PREDICATE are listed.

Non-nil arg DETAILS means show details about each file, in addition to
the file name.  It is passed to `diredp-list-file' (which see).

File names listed are absolute.  Mouseover gives help or an image-file
preview, and you can use `RET' or `mouse-2' to visit files."
  (unless bufname (setq bufname  (generate-new-buffer-name "*Files*")))
  (with-help-window bufname
    (princ "Files\n-----\n\n")
    (let ((all-files-no-wildcards  ())
          file-alist file-dir file+atts)
      (dolist (file  files)
        (unless (or (string= file "")    ; Ignore empty file names.
                    (and predicate  (not (funcall predicate file))))
          (if (not (diredp-string-match-p "[[?*]" file))
              (progn (setq file+atts  (diredp-list-file file details))
                     (unless (member file+atts all-files-no-wildcards)
                       (setq all-files-no-wildcards  (cons file+atts all-files-no-wildcards))))
            (setq file-dir    (or (file-name-directory file)  dir)
                  file-alist  (directory-files-and-attributes file-dir 'FULL "[[?*]" 'NOSORT))
            (dolist (_ff  file-alist)
              (setq file+atts  (diredp-list-file file details))
              (unless (member file+atts all-files-no-wildcards)
                (setq all-files-no-wildcards  (cons file+atts all-files-no-wildcards)))))))
      (save-excursion (dolist (fff  (nreverse all-files-no-wildcards))
                        (princ fff) (terpri)))))
  (with-current-buffer bufname
    (let ((buffer-read-only  nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line 3)
        (while (not (eobp))
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(mouse-face highlight help-echo diredp-mouseover-help dired-filename t
                                            ;; `keymap' does not work for Emacs 20.  Could use `local-map'
                                            ;; but that still leaves `RET' bound to `help-follow'.
                                            keymap diredp-list-files-map))
          (forward-line 1))))
    (set-buffer-modified-p nil)
    (setq buffer-read-only  t)
    (buffer-enable-undo)))


(defun diredp-y-or-n-files-p (prompt files &optional predicate details)
  "PROMPT user with a \"y or n\" question about a list of FILES.
Return t if answer is \"y\".  Otherwise, return nil.

Like `y-or-n-p', but you can also hit `l' to display the list of files
that the confirmation is for, in buffer `*Files'.  In that `'l'
listing you can mouseover to see image-file previews or use `RET' or
`mouse-2' to visit files.

When finished, buffer `*Files*' is killed if it was never shown, or is
hidden and buried otherwise.  Thus, if it was shown then it is still
available to revisit afterward (even if you quit using `C-g').

PREDICATE is passed to `diredp-list-files', to list only file names
for which it returns non-nil.

DETAILS is passed to `diredp-list-files', to show details about FILES."
  (let ((answer     'recenter)
        (orig-echo  (current-message)))
    (cond (noninteractive
           (setq prompt  (concat prompt
                                 (and (not (eq ?\   (aref prompt (1- (length prompt)))))  " ")
                                 "(y or n; l to show file list) "))
           (let ((temp-prompt  prompt))
             (while (not (memq answer '(act skip)))
               (let ((str  (read-string temp-prompt)))
                 (cond ((member str '("y" "Y")) (setq answer  'act))
                       ((member str '("n" "N")) (setq answer  'skip))
                       (t (setq temp-prompt  (concat "Please answer y or n.  " prompt))))))))
          ((if (not (fboundp 'display-popup-menus-p))
               (and window-system  (listp last-nonmenu-event)  use-dialog-box)
             (and (display-popup-menus-p)  (listp last-nonmenu-event)  use-dialog-box))
           (setq answer  (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
          (t
           (let ((list-buf        (generate-new-buffer-name "*Files*"))
                 (list-was-shown  nil))
             (unwind-protect
                 (progn
                   (define-key query-replace-map "l" 'show)
                   (setq prompt  (concat prompt
                                         (and (eq ?\   (aref prompt (1- (length prompt))))
                                              "" " ")
                                         "(y or n; l to show file list) "))
                   (while (let* ((reprompt-actions  '(recenter scroll-up scroll-down
                                                               scroll-other-window scroll-other-window-down))
                                 (key               (let ((cursor-in-echo-area  t))
                                                      (when minibuffer-auto-raise
                                                        (raise-frame (window-frame (minibuffer-window))))
                                                      (if (fboundp 'read-key)
                                                          (read-key (propertize
                                                                     (if (memq answer reprompt-actions)
                                                                         prompt
                                                                       (concat "Please answer y or n.  " prompt))
                                                                     'face 'minibuffer-prompt))
                                                        (read-char-exclusive
                                                         (if (memq answer reprompt-actions)
                                                             prompt
                                                           (concat "Please answer y or n.  " prompt)))))))
                            (setq answer  (lookup-key query-replace-map (vector key) t))
                            (cl-case answer
                              ((skip  act)              nil)
                              (recenter                 (recenter) t)
                              (show                     (diredp-list-files files nil list-buf predicate details)
                                                        (setq list-was-shown  t)) ; Record showing it.
                              (help                     (message "Use `l' to show file list") (sit-for 1))
                              (scroll-up                (condition-case nil (scroll-up-command) (error nil)) t)
                              (scroll-down              (condition-case nil (scroll-down-command) (error nil)) t)
                              (scroll-other-window      (condition-case nil (scroll-other-window) (error nil)) t)
                              (scroll-other-window-down (condition-case nil (scroll-other-window-down nil)
                                                          (error nil)) t)
                              ((exit-prefix  quit)      (signal 'quit nil) t)
                              (t (or (not (eq key ?\e))  (progn (signal 'quit nil) t)))))
                     (ding)
                     (discard-input)))
               (when (get-buffer list-buf)
                 (save-window-excursion (pop-to-buffer list-buf)
                                        (condition-case nil ; Ignore error if user already deleted.
                                            (if (one-window-p) (delete-frame) (delete-window))
                                          (error nil))
                                        (if list-was-shown (bury-buffer list-buf) (kill-buffer list-buf))))
               (define-key query-replace-map "l" nil)))))
    (let ((ret  (eq answer 'act)))
      (unless noninteractive
        (message "%s%c" prompt (if ret ?y ?n)) (sit-for 1)
        (let ((message-log-max  nil))
          (if orig-echo  (message "%s" orig-echo) (message ""))))
      ret)))

(defun diredp-copy-as-kill-from-clipboard (files)
  "Parse clipboard string FILES into file names, copy them to the kill ring.
Also record them in `diredp-last-copied-filenames'."
  (when (stringp files)
    (setq files  (split-string files "[\"\n]" t)))
  (let ((files-string  (mapconcat
                        (lambda (file)
                          (unless (file-exists-p file)
                            (message "No such file: `%s'" file))
                          file)
                        files
                        diredp-filename-separator)))
    (kill-new files-string)
    (setq diredp-last-copied-filenames  files-string)))


(defalias 'diredp-string-match-p 'string-match-p)
(defun dired-copy-filename-as-kill (&optional arg) ; Bound to `w', menu `Multiple' > `Copy Marked Names'
  "Copy names of marked (or next ARG) files into the kill ring.
Multiple file names are separated by the value of variable
`diredp-filename-separator'.

When multiple names are copied, those with space or quotes (', \") are
enclosed in double-quote chars if option
`diredp-quote-copied-filenames-flag' is non-nil.

With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdir name instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank].

The value of global variable `diredp-last-copied-filenames' is updated
to the string list of file name(s), so you can obtain it even after
the kill ring is modified."
  (interactive "P")
  (let* ((num-arg (prefix-numeric-value arg))
         (subdir  (dired-get-subdir))
         (files   (or (and subdir  (list subdir))
                      (cond ((not arg)       (dired-get-marked-files 'no-dir))
                            ((zerop num-arg) (dired-get-marked-files))
                            ((consp arg)     (dired-get-marked-files t))
                            (t               (dired-get-marked-files 'no-dir num-arg)))))
         (string  (or (and (not (cdr files))  (car files))
                      (mapconcat #'(lambda (file)
                                     (if (and diredp-quote-copied-filenames-flag
                                              (diredp-string-match-p "[ \"']" file))
                                         (format "%S" file)
                                       file))
                                 files
                                 diredp-filename-separator))))
    (unless (string= "" string)
      (if (eq last-command 'kill-region) (kill-append string nil) (kill-new string))
      (setq diredp-last-copied-filenames  (car kill-ring-yank-pointer))
      (message "%s" string))))

;;;###autoload
(defun diredp-copy-abs-filenames-as-kill ()
  "Copy the absolute names of the marked files in Dired to the kill ring.
Also set `diredp-last-copied-filenames' to the string listing those names.

Equivalent to `M-0 w' (`dired-copy-filename-as-kill' with prefix arg 0)."
  (interactive (diredp-ensure-mode))
  (dired-copy-filename-as-kill 0))

(defalias 'diredp-paste-files 'diredp-yank-files)
;;;###autoload
(defun diredp-yank-files (&optional dir no-confirm-p details)
  "Yank (paste) files to the current directory or DIR.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

The absolute names of the files to be yanked are taken from the
clipboard or, if that's empty, from names you've copied to the kill
ring using \\<dired-mode-map>\ `M-0 \\[dired-copy-filename-as-kill]' or \
\\[diredp-copy-abs-filenames-as-kill].

Those copy-filename commands also:
 * Use the value of option `diredp-filename-separator' to separate the
   copied file names.
 * Set variable `diredp-last-copied-filenames' to the same string.
   `diredp-yank-files' uses the value of that variable, not whatever
   is currently at the head of the kill ring.

\(To copy file names to the clipboard on MS Windows, you can use Windows
Explorer: Select the file names, then hold `Shift', right-click, and
choose `Copy as Path' from the menu.)

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to copy.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'."
  (interactive (list (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
                          (expand-file-name (read-directory-name "Yank files to directory: ")))
                     nil
                     (and current-prefix-arg
                          (<= (prefix-numeric-value current-prefix-arg) 0)
                          diredp-list-file-attributes)))
  (setq dir  (or dir  (and (derived-mode-p 'dired-mode)  (dired-current-directory))))
  (unless (file-directory-p dir) (error "Not a directory: `%s'" dir))
  (let* ((ipf-files  (funcall interprogram-paste-function))
         (files      (or ipf-files  diredp-last-copied-filenames)))
    (unless (stringp files)  (error "No copied file names"))
    (when ipf-files (setq files  (diredp-copy-as-kill-from-clipboard files)))
    (setq files  (diredp-delete-if-not (lambda (file) (file-name-absolute-p file))
                                       (split-string files diredp-filename-separator)))
    (unless files  (error "No copied *absolute* file names (Did you use `M-0 w'?)"))
    (if (and (not no-confirm-p)
             (diredp-y-or-n-files-p "Yank files whose names you copied? " files nil details))
        (dired-create-files #'dired-copy-file "Copy" files
                            (lambda (from) (expand-file-name (file-name-nondirectory from) dir)))
      (message "OK, file-yanking canceled"))))

;;;###autoload
(defun diredp-move-files-named-in-kill-ring (&optional dir no-confirm-p details)
  "Move files, whose absolute names you copied, to the current directory or DIR.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

You should have copied the list of file names as a string to the kill
ring using \\<dired-mode-map>`M-0 \\[dired-copy-filename-as-kill]' or \
\\[diredp-copy-abs-filenames-as-kill].
Those commands also set variable `diredp-last-copied-filenames' to the
same string.  `diredp-move-files-named-in-kill-ring' uses the value of
that variable, not whatever is currently at the head of the kill ring.

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to move.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'."
  (interactive (list (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
                          (expand-file-name (read-directory-name "Move files to directory: ")))
                     nil
                     (and current-prefix-arg
                          (<= (prefix-numeric-value current-prefix-arg) 0)
                          diredp-list-file-attributes)))
  (setq dir  (or dir  (and (derived-mode-p 'dired-mode)  (dired-current-directory))))
  (unless (file-directory-p dir) (error "Not a directory: `%s'" dir))
  (let ((files  diredp-last-copied-filenames))
    (unless (stringp files)  (error "No copied file names"))
    (setq files  (diredp-delete-if-not (lambda (file) (file-name-absolute-p file))
                                       (split-string files (regexp-quote diredp-filename-separator))))
    (unless files  (error "No copied (absolute* file names (Did you use `W'?)"))
    (if (and (not no-confirm-p)
             (diredp-y-or-n-files-p "MOVE files whose names you copied? " files nil details))
        (dired-create-files #'dired-rename-file "Move" files
                            (lambda (from) (expand-file-name (file-name-nondirectory from) dir)))
      (message "OK, file-moves canceled"))))

(provide 'diredp-files)
;;; diredp-files.el ends here
