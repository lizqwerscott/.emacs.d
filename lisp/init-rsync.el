(require 'multi-vterm)
(require 'vterm)

;; (add-to-list 'load-path
;;              "~/MyProject/emacs-plugin/rsync-project-mode/")

(require 'rsync-project-mode)

(setq rsync-project-default-auto-rsyncp t)
(setq rsync-project-default-gitignorep t)

(add-hook 'prog-mode-hook
          'rsync-project-mode)

(rsync-project-setup-indicator)

(defun project-remote-prefixed-buffer-name (mode)
  (concat "*"
          (if-let* ((proj (project-current nil)))
              (project-name proj)
            (file-name-nondirectory
             (directory-file-name default-directory)))
          "-"
          "remote"
          "-"
          (downcase mode)
          "*"))

(defun multi-vterm-project-remote ()
  "Connect project remote."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (vterm-buffer-name (project-remote-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (append display-buffer--same-window-action
                                            '((category . comint))))
      (vterm vterm-buffer-name)
      (when-let* ((remote-config (rsync-project-get-remote-config root))
                  (ssh-config (cl-getf remote-config :ssh-config)))
        (let ((remote-user (cl-getf ssh-config :user))
              (remote-host (cl-getf ssh-config :host))
              (remote-port (cl-getf ssh-config :port))
              (remote-dir (cl-getf ssh-config :remote-dir)))
          (vterm-send-M-w)
          (vterm-send-string (format "ssh -t %s %s 'cd ~/%s && exec $SHELL'"
                                     (if (and remote-user remote-host)
                                         (format "%s@%s"
                                                 remote-user
                                                 remote-host)
                                       remote-host)
                                     (if remote-port
                                         (if (not (= 22 remote-port))
                                             (format "-p %s" remote-port)
                                           "")
                                       "")
                                     remote-dir)
                             t)
          (vterm-send-return))))))

(defun multi-eshell-project-remote ()
  (interactive)
  (defvar eshell-buffer-name)
  (let* ((root (project-root (project-current t)))
         (eshell-buffer-name (project-remote-prefixed-buffer-name "eshell"))
         (eshell-buffer (get-buffer eshell-buffer-name)))
    (if (and eshell-buffer (not current-prefix-arg))
        (pop-to-buffer eshell-buffer (append display-buffer--same-window-action
                                             '((category . comint))))
      (when-let* ((remote-config (rsync-project-get-remote-config root))
                  (ssh-config (cl-getf remote-config :ssh-config)))
        (let* ((remote-user (cl-getf ssh-config :user))
               (remote-host (cl-getf ssh-config :host))
               (remote-port (cl-getf ssh-config :port))
               (remote-dir (cl-getf ssh-config :remote-dir))
               (default-directory (format "/ssh:%s%s:%s"
                                          (if (and remote-user remote-host)
                                              (format "%s@%s"
                                                      remote-user
                                                      remote-host)
                                            remote-host)
                                          (if remote-port
                                              (if (not (= 22 remote-port))
                                                  (format "#%s" remote-port)
                                                "")
                                            "")
                                          remote-dir))
               (eshell-prompt-function (lambda ()
                                         (concat (abbreviate-file-name (eshell/pwd))
                                                 (unless (eshell-exit-success-p)
                                                   (format " [%d]" eshell-last-command-status))
                                                 (if (= (file-user-uid) 0) " # " " $ ")))))
          (eshell t))))))

(defun project-remote-compilation-buffer-name-function (name-of-mode)
  (cond ((or (eq major-mode (intern-soft name-of-mode))
             (eq major-mode (intern-soft (concat name-of-mode "-mode"))))
  	     (buffer-name))
  	    (t
  	     (concat "*" "remote-" (downcase name-of-mode) "*"))))

(defun rsync-project-remote-compile ()
  (interactive)
  (when-let* ((root (project-root (project-current t)))
              (remote-config (rsync-project-get-remote-config root))
              (ssh-config (cl-getf remote-config :ssh-config)))
    (let* ((remote-user (cl-getf ssh-config :user))
           (remote-host (cl-getf ssh-config :host))
           (remote-port (cl-getf ssh-config :port))
           (remote-dir (cl-getf ssh-config :remote-dir))
           (default-directory (format "/ssh:%s%s:%s"
                                      (if (and remote-user remote-host)
                                          (format "%s@%s"
                                                  remote-user
                                                  remote-host)
                                        remote-host)
                                      (if remote-port
                                          (if (not (= 22 remote-port))
                                              (format "#%s" remote-port)
                                            "")
                                        "")
                                      remote-dir))
           (compilation-buffer-name-function #'project-remote-compilation-buffer-name-function))
      (call-interactively #'compile))))

(transient-define-suffix rsync-project-dispatch-term()
  (interactive)
  (call-interactively #'multi-vterm-project-remote))

(transient-define-suffix rsync-project-dispatch-eshell()
  (interactive)
  (require 'init-eshell)
  (call-interactively #'multi-eshell-project-remote))

(transient-define-suffix rsync-project-dispatch-compile()
  (interactive)
  (call-interactively #'rsync-project-remote-compile))

(transient-append-suffix 'rsync-project-dispatch '(-2 -1)
  ["Mics"
   :if rsync-project--check
   ("t" "Open Remote Term" rsync-project-dispatch-term)
   ("e" "Open Remote Eshell" rsync-project-dispatch-eshell)
   ("c" "Compile Remote" rsync-project-dispatch-compile)])

(provide 'init-rsync)
