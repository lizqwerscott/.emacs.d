;;; init-mcp.el --- init mcp packages                -*- lexical-binding: t; -*-

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

(require 'mcp-hub)

(require 'gptel)

(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools))
  (message "Register all mcp tool finished!"))

(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

(setq mcp-hub-servers
      `(,@(when-let* ((key (lizqwer/api-key-from-auth-source "api.github.com" "lizqwerscott^mcp")))
            `(("github" . (
                           :command "docker"
                           :args ("run" "--rm" "-i" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server")
                           :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,key)))))
        ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
        ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
        ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))))

(transient-define-prefix gptel-mcp-dispatch ()
  "Gptel mcp menu"
  [["Mcp server"
    ("s" "Start all server" mcp-hub-start-all-server)
    ("r" "Register all tool" gptel-mcp-register-tool)]
   ["Tools"
    ("a" "Active all" gptel-mcp-use-tool)
    ("c" "Deactivate all" gptel-mcp-close-use-tool)]]
  [("q" "Quit" transient-quit-all)])

(keymap-sets gptel-mode-map
             '(("C-c m" . gptel-mcp-dispatch)))

(provide 'init-mcp)
;;; init-mcp.el ends here
