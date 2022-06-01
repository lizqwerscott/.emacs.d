;;; evil-leader-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-leader" "evil-leader.el" (0 0 0 0))
;;; Generated autoloads from evil-leader.el

(autoload 'global-evil-leader-mode "evil-leader" "\
Global minor mode for <leader> support.

This is a minor mode.  If called interactively, toggle the
`Global evil-leader mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `global-evil-leader-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'evil-leader-mode "evil-leader" "\
Minor mode to enable <leader> support.

This is a minor mode.  If called interactively, toggle the
`Evil-leader mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-leader-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'evil-leader/set-key "evil-leader" "\
Bind `key' to command `def' in `evil-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs.

\(fn KEY DEF &rest BINDINGS)" t nil)

(autoload 'evil-leader/set-key-for-mode "evil-leader" "\
Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-leader/set-key'.

\(fn MODE KEY DEF &rest BINDINGS)" t nil)

(register-definition-prefixes "evil-leader" '("evil-leader"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; evil-leader-autoloads.el ends here
