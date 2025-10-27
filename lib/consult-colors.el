;;; consult-colors.el --- select colors use consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

;;

;;; Code:

(require 'embark)
(require 'consult)

(require 'shr-color)

(defvar consult-colors-history nil
  "History for `consult-colors-emacs' and `consult-colors-web'.")

;; No longer preloaded in Emacs 28.
(autoload 'list-colors-duplicates "facemenu")
;; No preloaded in consult.el
(autoload 'consult--read "consult")

;;;###autoload
(defun consult-colors-emacs (color)
  "Show a list of all supported colors for a particular frame.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
  (interactive
   (list (consult--read (list-colors-duplicates (defined-colors))
                        :prompt "Emacs color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))

;; Adapted from counsel.el to get web colors.
(defun counsel-colors--web-list nil
  "Return list of CSS colors for `consult-colors-web'."
  (require 'shr-color)
  (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

;;;###autoload
(defun consult-colors-web (color)
  "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
  (interactive
   (list (consult--read (counsel-colors--web-list)
                        :prompt "Color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))

;;; embark

(defun rounding-numbers (list-of-num decimal-points)
  "Return a list of numbers rounded to DECIMAL-POINTS decimal places.

LIST-OF-NUM is a list of numbers to round.
DECIMAL-POINTS is the number of decimal places to round to.

Return a list of floats rounded to the specified precision."
  (let ((rounding (expt 10 decimal-points)))
    (mapcar (lambda (x) (/ (fround (* rounding x)) rounding)) list-of-num)))

(defun numbers-to-string (list-of-num separator)
  "Convert LIST-OF-NUM to a string with SEPARATOR between elements.

LIST-OF-NUM is a list of numbers to convert.
SEPARATOR is the string to use as separator between numbers.

Return a string with numbers separated by SEPARATOR."
  (mapconcat #'number-to-string list-of-num separator))

;; Colors RGB number as string
(defvar color-rgb-round-decimal-points 2 "Number of decimal points to round RGB colors.")
(defvar color-rgb-string-separator "," "SEPARATOR between numbers for RGB strings.")

(defun color-name-to-rgb-string (name)
  "Return RGB value of color NAME as string with components between 0 and 1.

NAME is a color name.

Return a string of the form \"R,G,B\" where each component is between 0 and 1.
Return nil if NAME does not designate a valid color."
  (when-let* ((rgb (color-name-to-rgb name)))
    (numbers-to-string rgb color-rgb-string-separator)))

(defun color-name-to-round-rgb-string (name)
  "Return rounded RGB value of color NAME as string.

NAME is a color name.

Return a string of the form \"R,G,B\" where each component is rounded
to `color-rgb-round-decimal-points' decimal places.
Return nil if NAME does not designate a valid color."
  (when-let* ((rgb (color-name-to-rgb name)))
    (numbers-to-string (rounding-numbers rgb color-rgb-round-decimal-points)
                       color-rgb-string-separator)))

;; Adapted from counsel.el to convert color name to hex.
(defun counsel-colors--hex (name)
  "Return hexadecimal value of color NAME.

NAME is a color name.

Return the hexadecimal color value as a string.
Return nil if NAME does not designate a valid color."
  (when-let* ((rgb (color-name-to-rgb name))
              ;; Sets 2 digits per component.
              (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
    hex))

(defvar-keymap embark-consult-color-action-map
  :doc "Keymap for embark actions in the `color' category of marginalia.")

;; Kill and insert versions
(defvar embark-consult-color-functions-alist
  '(((color-name-to-round-rgb-string . "rRGB") . ("r" . "k"))
    ((color-name-to-rgb-string       . "RGB")  . ("R" . "K"))
    ((counsel-colors--hex            . "hex")  . ("h" . "H")))
  "Alist of ((FUN . DESC) . (INSERT-BIND . KILL-BIND)) for color functions.

FUN is a function that converts a color name to some value.
DESC is a string describing the function.
INSERT-BIND is the keybinding for the insert action.
KILL-BIND is the keybinding for the kill action.

Used to define `insert' and `kill-new' versions for embark actions.")

;; Define `insert' versions
(cl-loop for fun in embark-consult-color-functions-alist do
         (let* ((sym (caar fun))
                (bind (cadr fun))
                (desc (format "Insert %s" (cdar fun)))
                (newname (intern (format "%s-insert" (symbol-name sym)))))
           (fset newname `(lambda (color)
                            (insert (,sym color))
                            (push color consult-colors-history)))
           (define-key embark-consult-color-action-map (kbd bind) (cons desc newname))))

;; Define `kill-new' versions
(cl-loop for fun in embark-consult-color-functions-alist do
         (let* ((sym (caar fun))
                (bind (cddr fun))
                (desc (format "Kill %s" (cdar fun)))
                (newname (intern (format "%s-kill" (symbol-name sym)))))
           ;; `(lambda (color) (kill-new (apply ',fun (list color))))
           (fset newname `(lambda (color)
                            (kill-new (,sym color))
                            (push color consult-colors-history)))
           (define-key embark-consult-color-action-map (kbd bind) (cons desc newname))))

(add-to-list 'embark-keymap-alist '(color . embark-consult-color-action-map))

(provide 'consult-colors)
;;; consult-colors.el ends here
