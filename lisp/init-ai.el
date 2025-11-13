;;; init-ai.el --- init ai package                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp, ai

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

(require 'init-gptel)

(when user/aider
  (require 'init-aider))

(require 'init-macher)

(defun set-ai-completion (symbol value)
  "Set font SYMBOL VALUE."
  (dolist (mode '(python-ts-mode rust-ts-mode c++-ts-mode web-mode bash-ts-mode go-ts-mode csharp-mode csharp-ts-mode))
    (remove-hook (intern (concat (symbol-name mode) "-hook"))
                 #'copilot-mode))
  (remove-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (remove-hook 'prog-mode-hook #'wingman-mode)
  (set-default-toplevel-value symbol value)
  (wait-packages!
   (pcase value
     ('copilot
      '((copilot :fetcher github
                 :repo "zerolfx/copilot.el"
                 :branch "main")))
     ('minuet
      '((minuet :fetcher github
                :repo "milanglacier/minuet-ai.el")))
     ('wingman
      '((wingman :fetcher github :repo "mjrusso/wingman")))))
  (pcase value
    ('copilot
     (require 'init-copilot))
    ('minuet
     (require 'init-minuet-ai))
    ('wingman
     (add-hook 'prog-mode-hook
               #'wingman-mode))))

(defcustom user/ai-completion nil
  "Use what ai to completion: copilot, minuet, wingman."
  :group 'user
  :type '(choice (const :tag "none" nil)
                 (const :tag "copilot" copilot)
                 (const :tag "minuet" minuet)
                 (const :tag "wingman" wingman))
  :set #'set-ai-completion)

(defun ai-complete ()
  "Complete with the ai in corfu."
  (pcase user/ai-completion
    ('copilot
     (copilot-accept-completion))
    ('minuet
     (minuet-accept-suggestion))
    ('wingman
     (wingman-accept-full))))

(require 'init-mcp)

(provide 'init-ai)
;;; init-ai.el ends here
