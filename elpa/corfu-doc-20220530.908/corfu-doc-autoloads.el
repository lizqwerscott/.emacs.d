;;; corfu-doc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu-doc" "corfu-doc.el" (0 0 0 0))
;;; Generated autoloads from corfu-doc.el

(defvar corfu-doc-mode nil "\
Non-nil if Corfu-doc mode is enabled.
See the `corfu-doc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-doc-mode'.")

(custom-autoload 'corfu-doc-mode "corfu-doc" nil)

(autoload 'corfu-doc-mode "corfu-doc" "\
Corfu doc minor mode.

This is a global minor mode.  If called interactively, toggle the
`Corfu-doc mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-doc-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'corfu-doc-scroll-up "corfu-doc" "\


\(fn &optional N)" t nil)

(autoload 'corfu-doc-scroll-down "corfu-doc" "\


\(fn &optional N)" t nil)

(autoload 'corfu-doc-toggle "corfu-doc" "\
Toggles the doc popup display or hide.

When using this command to manually hide the doc popup, it will
not be displayed until this command is called again. Even if the
corfu doc mode is turned on and `corfu-doc-auto' is set to Non-nil." t nil)

(autoload 'toggle-corfu-doc-mode "corfu-doc" "\
Toggles corfu doc mode on or off.
With optional ARG, turn corfu doc mode on if and only if ARG is positive.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "corfu-doc" '("corfu-doc-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; corfu-doc-autoloads.el ends here
