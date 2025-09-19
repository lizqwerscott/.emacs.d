;;; lib-hydra.el --- lib hydra                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

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

(require 'hydra)
(require 'pretty-hydra)
(require 'posframe)
(require 'lib-elisp-utils)

(defface posframe-border
  `((t (:inherit region)))
  "Face used by the `posframe' border."
  :group 'posframe)

(defvar posframe-border-width 2
  "Default posframe border width.")

(defun posframe-poshandler-frame-center-near-bottom (info)
  "Position a posframe near the bottom center of the parent frame.

INFO is a plist containing information about the parent frame and posframe.
Returns a cons cell (X . Y) representing the position coordinates."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (/ (+ (plist-get info :parent-frame-height)
              (* 2 (plist-get info :font-height)))
           2)))

(defun hydra-set-posframe-show-params ()
  "Set hydra-posframe style."
  (setq hydra-posframe-show-params
        `( :left-fringe 8
           :right-fringe 8
           :internal-border-width 2
           :internal-border-color ,(face-background 'posframe-border nil t)
           :background-color ,(face-background 'tooltip nil t)
           :foreground-color ,(face-foreground 'tooltip nil t)
           :lines-truncate t
           :poshandler posframe-poshandler-frame-center-near-bottom)))
(hydra-set-posframe-show-params)

(defun start-posframe ()
  "Start posframe."
  (setq hydra-hint-display-type
        'posframe))

(defun stop-posframe ()
  "Stop posframe."
  (setq hydra-hint-display-type
        'lv)
  (posframe-delete-all))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))


(cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                    &key face height v-adjust)
  "Add an icon in the hydra TITLE.

- ICON-TYPE is icon type
- ICON-NAME is icon name
- FACE is title face
- HEIGHT is title height
- V-ADJUST is adjust"
  (let ((face (or face 'mode-line-emphasis))
        (height (or height 1.2))
        (v-adjust (or v-adjust 0.0)))
    (concat
     (when (and (icons-displayable-p) icon-type icon-name)
       (let ((f (intern (format "nerd-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
     (propertize title 'face face))))

(defun pretty-hydra-define-add-exit (head-plist)
  "Add :exit for HEAD-PLIST."
  (mapcar-if #'(lambda (head)
                 (mapcar-if-not #'(lambda (key)
                                    (append key
                                            (list :exit t)))
                                head
                                #'(lambda (v)
                                    (cl-find :exit v :test #'eq))))
             head-plist
             #'listp))

;;;###autoload
(defmacro pretty-hydra-define-e (name body head-plist)
  "Define hydra menu.
NAME is hydra menu name.
BODY is hydra menu body.
HEAD-PLIST is hydra menu head-plist."
  (declare (indent 1) (debug (form def-body)))
  `(progn
     (pretty-hydra-define ,name ,body
                          ,(if (cl-getf body :all-exit)
                               (pretty-hydra-define-add-exit head-plist)
                             head-plist))
     ,@(when (cl-getf body :posframe)
         (let ((name (symbol-name name)))
           `((advice-add #',(intern (concat name "/body")) :before #'start-posframe)
             (advice-add #',(intern (concat name "/nil")) :after #'stop-posframe)) ))))

(provide 'lib-hydra)
;;; lib-hydra.el ends here
