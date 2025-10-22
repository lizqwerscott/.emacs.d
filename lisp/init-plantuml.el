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
  (defun plantuml-convert (path output-type)
    "Plantuml convert.
PATH is convert path.
OUTPUT-TYPE is export file type."
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
      (shell-command command res-buffer res-buffer)))

  (defun plantuml-export (&optional output-type)
    "Plantuml export.
OUTPUT-TYPE is export file type."
    (interactive "P")
    (when-let* ((file-path (buffer-file-name)))
      (plantuml-convert file-path
                        (when output-type
                          (completing-read "Select Type:" '("svg" "png" "latex" ("EPS" . "eps") ("ASCII" . "xt")))))))

  (defun plantuml-convert-dir (dir &optional output-type)
    "Plantuml export.
OUTPUT-TYPE is export file type."
    (interactive (list
                  (read-directory-name "Select Dir:")
                  current-prefix-arg))
    (when (plantuml-convert dir
                            (when output-type
                              (completing-read "Select Type:" '("svg" "png" "latex" ("EPS" . "eps") ("ASCII" . "xt")))))
      (dired-other-window dir)))

  (keymap-binds plantuml-mode-map
    ("C-c C-e" . plantuml-export)))

(autoload #'plantuml-convert-dir "plantuml-mode" nil t)

(provide 'init-plantuml)
;;; init-plantuml.el ends here
