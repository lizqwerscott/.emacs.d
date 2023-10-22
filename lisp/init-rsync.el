(require 'multi-vterm)
(require 'vterm)

(add-to-list 'load-path
             "/home/lizqwer/MyProject/emacs-plugin/rsync-project-mode/")

(require 'rsync-project-mode)

(setq rsync-project-sync-on-save t)

(add-hook 'prog-mode-hook
          'rsync-project-mode)

(defun multi-vterm-project-remote ()
  "Connect project remote."
  (interactive)
  (multi-vterm-project)
  (let ((remote-config (rsync-project-get-remote-config (project-root (project-current)))))
    (let ((remote-user (first (second remote-config)))
          (remote-host (second (second remote-config)))
          (remote-port (third (second remote-config))))
      (vterm-send-M-w)
      (vterm-send-string (format "ssh %s %s"
                                 (if (and remote-user remote-host)
                                     (format "%s@%s"
                                             remote-user
                                             remote-host)
                                   remote-host)
                                 (if remote-port
                                     (if (not (= 22 remote-port))
                                         (format "-p %s" remote-port)
                                       "")
                                   ""))
                         t)
      (vterm-send-return))))

(provide 'init-rsync)
