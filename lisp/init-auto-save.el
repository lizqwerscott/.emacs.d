(add-hook 'prog-mode-hook (lambda ()
			                (require 'auto-save)
			                (auto-save-enable)
			                (setq auto-save-silent t
                                  auto-save-delete-trailing-whitespace t
				                  auto-save-idle 1)
			                ;; (setq auto-save-disable-predicates
				            ;;       '((lambda ()
				            ;;           (eq major-mode 'ron-mode))))
                            ))

(provide 'init-auto-save)
