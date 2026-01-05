;;; project-color.el --- project color               -*- lexical-binding: t; -*-

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

;; from https://amitp.blogspot.com/2025/04/emacs-per-project-colors.html

;;; Code:

(require 'project)
(require 'color)

(defgroup project-color nil
  "Project color."
  :group 'tools)

(defcustom project-color-function #'project-color-background-grouped
  "Function to determine background color for current project.

This function should take no arguments and return a color string in
hexadecimal format (e.g., \"#RRGGBB\").

Available functions:
- `project-color-background': Simple hash-based color
- `project-color-background-by-hierarchy': Color based on directory hierarchy
- `project-color-background-by-tree': Color based on directory tree structure
- `project-color-background-grouped': Color grouped by parent directory
categories

See each function's documentation for details on their algorithms."
  :type 'function
  :group 'project-color)

(defun project-color-background ()
  "Return a background color for the project containing this directory."
  (let* ((project (project-current))
         (dirhash (sxhash (if project (project-root project) default-directory)))
         (hue (/ (mod dirhash 1000) 1000.0))
         (saturation (+ 0.3 (* 0.1 (mod (/ dirhash 1000) 3))))
         (lightness (+ 0.4 (* 0.05 (mod (/ (/ dirhash 1000) 3) 4))))
         (rgb (color-hsl-to-rgb hue saturation lightness)))
    (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2)))

(defun project-color-background-by-hierarchy ()
  "Return a background color based on directory hierarchy."
  (let* ((project (project-current))
         (dir (if project (project-root project) default-directory))
         (home (expand-file-name "~"))
         (relative-dir (if (string-prefix-p home dir)
                           (substring dir (length home))
                         dir))
         (parts (split-string relative-dir "/" t))
         (hash-base 0))

    (cl-loop for part in parts
             for i from 1
             do (setq hash-base
                      (logxor hash-base
                              (ash (sxhash part) (* i 2)))))

    (let* ((dirhash (abs hash-base))
           (base-hue (/ (mod (sxhash (or (car parts) dir)) 1000) 1000.0))
           (depth (length parts))
           (hue-adjust (/ (mod dirhash 200) 2000.0))
           (hue (mod (+ base-hue hue-adjust) 1.0))
           (saturation (+ 0.3 (* 0.1 (mod depth 3))))
           (lightness (+ 0.4 (* 0.05 (mod depth 4))))
           (rgb (color-hsl-to-rgb hue saturation lightness)))
      (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2))))

(defun project-color-background-by-tree ()
  "Return a color based on directory tree structure."
  (let* ((project (project-current))
         (dir (if project (project-root project) default-directory))
         (home (expand-file-name "~"))
         (relative-dir (if (string-prefix-p home dir)
                           (substring dir (length home))
                         dir))
         (parts (split-string relative-dir "/" t)))

    (when (null parts) (setq parts (list "root")))

    (let* ((primary (or (car parts) "default"))
           (secondary (or (cadr parts) primary))
           (primary-hash (sxhash primary))
           (secondary-hash (sxhash secondary))

           (base-hue (/ (mod primary-hash 360) 360.0))

           (hue-offset (/ (mod secondary-hash 30) 360.0))
           (hue (mod (+ base-hue hue-offset) 1.0))

           (saturation (+ 0.35 (* 0.1 (mod secondary-hash 3))))
           (lightness (+ 0.45 (* 0.05 (mod (+ primary-hash secondary-hash) 4))))

           (rgb (color-hsl-to-rgb hue saturation lightness)))
      (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2))))

(defun project-color-background-grouped ()
  "Group projects by color categories based on parent directory."
  (let* ((project (project-current))
         (dir (if project (project-root project) default-directory))
         (home (expand-file-name "~"))
         (relative-dir (if (string-prefix-p home dir)
                           (substring dir (length home))
                         dir))
         (parts (split-string relative-dir "/" t))
         (primary (or (car parts) "root"))
         (category-hash (sxhash primary)))

    (let* ((color-groups '((0.0  "Red")     ; 0-59
                           (0.166 "Orange-Yellow") ; 60-119
                           (0.333 "Green")   ; 120-179
                           (0.5   "Cyan")    ; 180-239
                           (0.666 "Blue")    ; 240-299
                           (0.833 "Purple"))) ; 300-359

           (group-index (mod category-hash (length color-groups)))
           (base-hue (car (nth group-index color-groups)))

           (subdir (or (cadr parts) primary))
           (sub-hash (sxhash subdir))
           (hue-adjust (/ (mod sub-hash 60) 360.0))
           (hue (mod (+ base-hue hue-adjust) 1.0))

           (depth (length parts))
           (saturation (+ 0.4 (* 0.1 (mod depth 2))))
           (lightness (+ 0.5 (* 0.05 (mod depth 3))))

           (rgb (color-hsl-to-rgb hue saturation lightness)))

      (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2))))

(require 'face-remap)
(defvar-local project-color-face-remap-cookies nil
  "List of face remap cookies for this buffer.")

(defun project-color-reset-modeline-colors ()
  "Reset all modified faces to default."
  (when project-color-face-remap-cookies
    (dolist (cookie project-color-face-remap-cookies)
      (face-remap-remove-relative cookie))))

(defun project-color-set-modeline-color ()
  "Set mode line color based on current buffer's project."
  (project-color-reset-modeline-colors)
  (let ((color (funcall project-color-function)))
    (push (face-remap-add-relative 'tab-line-tab-current :background color :foreground "white")
          project-color-face-remap-cookies)
    (push (face-remap-add-relative 'mode-line-active :background color :foreground "white") project-color-face-remap-cookies)
    (push (face-remap-add-relative 'line-number-current-line :background color :foreground "white") project-color-face-remap-cookies)))

;;;###autoload
(define-minor-mode project-color-mode
  "Toggle project color mode."
  :lighter nil
  (if project-color-mode
      (project-color-set-modeline-color)
    (project-color-reset-modeline-colors)))

;;;###autoload
(define-minor-mode global-project-color-mode
  "Toggle project color mode."
  :lighter nil
  :global t
  :init-value nil
  :group 'project-color
  (if global-project-color-mode
      (progn
        (add-hook 'find-file-hook #'project-color-mode)
        (add-hook 'dired-mode-hook #'project-color-mode)
        (add-hook 'change-major-mode-hook #'project-color-mode)
        (add-hook 'temp-buffer-setup-hook #'project-color-mode))
    (remove-hook 'find-file-hook #'project-color-mode)
    (remove-hook 'dired-mode-hook #'project-color-mode)
    (remove-hook 'change-major-mode-hook #'project-color-mode)
    (remove-hook 'temp-buffer-setup-hook #'project-color-mode)))

(provide 'project-color)
;;; project-color.el ends here
