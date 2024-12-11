(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-send-changes-idle-time 0.5)

(require 'eglot)

(add-hook 'prog-mode-hook
          #'(lambda ()
              (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                (eglot-ensure))))

(add-hooks '(markdown-mode yaml-mode yaml-ts-mode)
           #'eglot-ensure)

(require 'consult-eglot)
(keymap-set eglot-mode-map
            "C-M-."
            #'consult-eglot-symbols)

;; Emacs LSP booster
(when (executable-find "emacs-lsp-booster")
  (unless (package-installed-p 'eglot-booster)
    (and (fboundp #'package-vc-install)
       (quelpa '(eglot-booster :fetcher github :repo "jdtsmith/eglot-booster"))))
  (autoload #'eglot-booster-mode "eglot-booster" nil t)
  (eglot-booster-mode 1))

(provide 'init-eglot)
