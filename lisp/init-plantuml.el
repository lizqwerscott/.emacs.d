;;; init-plantuml.el --- plantuml                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages! '(plantuml-mode))

;; for clang-uml
(add-to-list 'auto-mode-alist '("\\.clang-uml\\'" . yaml-ts-mode))

;; Enable plantuml-mode for Puml files
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(setopt plantuml-jar-path (expand-file-name "var/plantuml/plantuml.jar"
                                            user-emacs-directory)
        plantuml-default-exec-mode 'jar)

(when sys/macp
  (setopt plantuml-java-command "/opt/homebrew/opt/openjdk/bin/java"))

;; for org
(setopt org-plantuml-jar-path plantuml-jar-path)

(org-babel-do-load-languages
 'org-babel-load-languages
 `(,@org-babel-load-languages
   (plantuml . t)))

(with-eval-after-load 'plantuml-mode
  (defun plantuml-convert (path output-type &optional finish-callback)
    "Plantuml convert.
PATH is convert path.
OUTPUT-TYPE is export file type.
FINISH-CALLBACK is callback when process finish."
    (when-let* ((path (when (file-exists-p path)
                        path))
                (command (format "%s -jar %s %s %s"
                                 plantuml-java-command
                                 plantuml-jar-path
                                 (if output-type
                                     (format "-t%s"
                                             output-type)
                                   "")
                                 path))
                (res-buffer (get-buffer-create "*Plantuml Export*")))
      (make-process
       :name "plantuml-convert"
       :buffer res-buffer
       :command (list "bash" "-c" command)
       :sentinel (lambda (process event)
                   (when (string-match-p "finished\\|exited" event)
                     (if finish-callback
                         (funcall finish-callback
                                  (cond ((string-match-p "finished" event) 'finished)
                                        ((string-match-p "exited abnormally" event)
                                         'error)
                                        (t 'other))
                                  event)
                       (message "PlantUML conversion completed: %s"
                                (cond ((string-match-p "finished" event) "success")
                                      ((string-match-p "exited abnormally" event) "error: ")
                                      (t event))))))
       :noquery t)))

  (defun plantuml-export (&optional output-type)
    "Plantuml export.
OUTPUT-TYPE is export file type."
    (interactive "P")
    (when-let* ((file-path (buffer-file-name))
                (output-type (if output-type
                                 (completing-read "Select Type:"
                                                  '("svg" "png" "latex" ("EPS" . "eps") ("ASCII" . "xt")))
                               "png")))
      (knockknock-notify :title "Export..."
                         :message (format "Export %s.%s to %s"
                                          (file-name-base file-path)
                                          (file-name-extension file-path)
                                          output-type)
                         :icon "fa-refresh")
      (plantuml-convert file-path
                        output-type
                        (lambda (exit-status event)
                          (if (eq exit-status 'finished)
                              (knockknock-notify :title "Export finished!" :icon "cod-check")
                            (knockknock-alert :title "Export Error!" :icon "cod-error"))))))

  (defun plantuml-convert-dir (dir &optional output-type)
    "Plantuml export.
OUTPUT-TYPE is export file type."
    (interactive (list
                  (read-directory-name "Select Dir:")
                  current-prefix-arg))
    (when-let* ((output-type (if output-type
                                 (completing-read "Select Type:"
                                                  '("svg" "png" "latex" ("EPS" . "eps") ("ASCII" . "xt")))
                               "png")))
      (knockknock-notify :title "Convert..."
                         :message (format "convert %s to %s" (file-name-base dir) output-type)
                         :icon "fa-refresh")
      (plantuml-convert dir
                        output-type
                        (lambda (exit-status event)
                          (if (eq exit-status 'finished)
                              (progn
                                (knockknock-notify :title "Export finished!" :icon "cod-check")
                                (dired-other-window dir))
                            (knockknock-alert :title "Export Error!" :icon "cod-error"))))))

  (keymap-binds plantuml-mode-map
    ("C-c C-e" . plantuml-export)))

(autoload #'plantuml-convert-dir "plantuml-mode" nil t)

(provide 'init-plantuml)
;;; init-plantuml.el ends here
