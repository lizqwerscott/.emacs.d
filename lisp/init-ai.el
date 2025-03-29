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

;; (require 'init-codegeex)
(pcase user/ai-completion
  ('copilot
   (require 'init-copilot))
  ('codeium
   (require 'init-codeium))
  ('minuet
   (require 'init-minuet-ai)))

(defun ai-complete ()
  (pcase user/ai-completion
    ('copilot
     (copilot-accept-completion))
    ('codeium
     (codeium-overlay-tab-command))
    ('minuet
     (minuet-accept-suggestion))))

(require 'init-mcp)

(provide 'init-ai)
;;; init-ai.el ends here
