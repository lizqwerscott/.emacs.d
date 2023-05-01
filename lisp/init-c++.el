;;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(provide 'init-c++)
