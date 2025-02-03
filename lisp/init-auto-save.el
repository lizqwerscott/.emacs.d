(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t
      auto-save-delete-trailing-whitespace t
	  auto-save-idle 1)

(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "gpg"
           (file-name-extension (buffer-name)) t))))

(provide 'init-auto-save)
