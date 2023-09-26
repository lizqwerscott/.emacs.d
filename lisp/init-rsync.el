(require 'project)


(defun rsync-project ()
  "set project rsync to remote"
  (interactive)
  (let* ((project-root-dir (project-root (project-current)))
         (dir-locals-name (concat project-root-dir ".dir-locals.el")))
    (find-file dir-locals-name)
    (insert ";;; Directory Local Variables
;;; For more information see (info \"(emacs) Directory Variables\")

((nil . ((rsync-remote-host . \"host\")
         (rsync-remote-port . 22)
         (rsync-remote-path . \"/\")
         (rsync-remote-name . \"lizqwer\")
         (rsync-local-path . \"/path/to/local/directory/\")
         (rsync-excluded-dirs . (\".git\" \"*.egg-info\"))
         (rsync-sync-on-save . t))))")
    ))

(defun rsync-all ()
  "rsync all"
  (interactive)
  (let ((rsync-buffer (get-buffer-create "*Rsync project*")))
    (async-shell-command (concat "rsync -avtP "
                                 (if (not (= 22 rsync-remote-port))
                                     (concat "-e "
                                             "\""
                                             "ssh -p "
                                             (number-to-string rsync-remote-port)
                                             "\"")
                                   "")
                                 " "
                                 (string-join
                                  (mapcar #'(lambda (dir)
                                              (concat "--exclude=" dir))
                                          rsync-excluded-dirs)
                                  " ")
                                 " "
                                 rsync-local-path " "
                                 rsync-remote-name "@" rsync-remote-host ":" rsync-remote-path)
                         rsync-buffer)))

(provide 'init-rsync)
