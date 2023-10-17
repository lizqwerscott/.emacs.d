(require 'project)
(require 'f)

(defcustom rsync-project-list-file (f-join no-littering-var-directory "rsync-project-list-file.el")
  "File in which to save the list of known projects."
  :type 'file
  :group 'rsync)

(defvar rsync-project-remote-list nil
  "List of project rsync remote server.")

(defun rsync-project-write-list ()
  "Save the rsync project remote list."
  (let ((filename rsync-project-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp rsync-project-remote-list
            (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun rsync-project-read-list ()
  "Load the rsync project remote list."
  (let ((filename rsync-project-list-file))
    (setq rsync-project-remote-list
          (when (file-exists-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (read (current-buffer)))))
    (unless (seq-every-p
             (lambda (elt) (stringp (car-safe elt)))
             rsync-project-remote-list)
      (warn "Contents of %s are in wrong format, resetting"
            rsync-project-list-file)
      (setq rsync-project-remote-list nil))))

(defun rsync-get-project-ssh-config (project-path)
  (find (file-truename project-path)
        rsync-project-remote-list
        :key #'(lambda (elm)
                 (file-truename
                  (first elm)))
        :test #'string=))

(defun rsync-project-ssh-config-cmd (ssh-config)
  (let ((local-path (first ssh-config))
        (remote-user (first (second ssh-config)))
        (remote-host (second (second ssh-config)))
        (remote-port (third (second ssh-config)))
        (remote-path (fourth (second ssh-config)))
        (ignore-list (third ssh-config)))
    (format "rsync -avtP %s %s %s %s"
            (if remote-port
                (if (not (= 22 remote-port))
                    (format "-e \"ssh -p %d \"" remote-port)
                  )
              "")
            (string-join
             (mapcar #'(lambda (dir)
                         (concat "--exclude=" dir))
                     ignore-list)
             " ")
            local-path
            (if (and remote-user remote-host)
                (format "%s@%s:%s"
                        remote-user
                        remote-host
                        remote-path)
              (format "%s:%s"
                      remote-host
                      remote-path)))))

;; (tramp-file-name ssh nil nil pveubuntu nil projects/ nil)
;; (tramp-file-name ssh lizqwer nil 10.0.96.4 2223 projects/ nil)

;;;###autoload
(defun rsync-add-project ()
  "Add now project to rsync list"
  (interactive)
  (let ((project-root-dir (file-truename (project-root (project-current))))
        (name (project-name (project-current))))
    (if (not (rsync-get-project-ssh-config project-root-dir))
        (let ((ignore-file-list (list ".git"))
              (remote-dir (tramp-dissect-file-name (read-file-name "Remote dir:" "/ssh:")))
              (add-ignore-filep (yes-or-no-p "Add ignore files:")))
          (let ((remote-dir-path (f-join "~/" (tramp-file-name-localname remote-dir))))
            (when (not (string= (file-name-base remote-dir-path)
                              name))
              (setf remote-dir-path
                    (f-join remote-dir-path name)))
            (while add-ignore-filep
              (add-to-list 'ignore-file-list
                           (f-filename (read-file-name "Ignore path:" project-root-dir)))
              (setf add-ignore-filep
                    (yes-or-no-p (format "(%s)Add ignore files:" ignore-file-list))))
            (add-to-list 'rsync-project-remote-list
                         (list project-root-dir
                               (list (tramp-file-name-user remote-dir)
                                     (tramp-file-name-host remote-dir)
                                     (tramp-file-name-port remote-dir)
                                     remote-dir-path)
                               ignore-file-list))
            (rsync-project-write-list)))
      (message "Already add now project."))))

;;;###autoload
(defun rsync-remove-project ()
  "Remove now project in rsync list"
  (interactive)
  (rsync-project-read-list)
  (let ((ssh-config (rsync-get-project-ssh-config (project-root (project-current)))))
    (if ssh-config
        (progn
          (setf rsync-project-remote-list
                (cl-remove-if #'(lambda (item)
                                  (string= (first item)
                                           (first ssh-config)))
                              rsync-project-remote-list))
          (rsync-project-write-list))
      (message "Now project not add rsync"))))

;;;###autoload
(defun rsync-all ()
  "Rsync all."
  (interactive)
  (let ((ssh-config (rsync-get-project-ssh-config (project-root (project-current)))))
    (if ssh-config
        (let ((rsync-buffer (get-buffer-create "*Rsync project*")))
          (async-shell-command (rsync-project-ssh-config-cmd ssh-config)
                               rsync-buffer))
      (message "Need use rsync-add-project"))))

;;;###autoload
(defun rsync-project-show-ssh-config ()
  (interactive)
  (rsync-project-read-list)
  (message "%s" (rsync-get-project-ssh-config (project-root (project-current)))))

(rsync-project-read-list)

(defun test ()
  (interactive)
  (rsync-project-read-list)
  ;; (message "%s" (rsync-project-ssh-config-cmd (rsync-get-project-ssh-config (project-root (project-current)))))
  ;; (message "%s, %s" (project-root (project-current)) (rsync-get-project-ssh-config (project-root (project-current))) )
  ;; (message "%s" (yes-or-no-p "hh:"))
  )

(provide 'init-rsync)
