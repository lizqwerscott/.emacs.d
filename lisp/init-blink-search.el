(require 'blink-search)
(add-hook 'blink-search-mode-hook #'meow-insert-mode)
(setq blink-search-search-backends
      '("Buffer List" "Find File" "Common Directory" "Recent File" "EAF Browser History" "Google Suggest"))

(setq blink-search-common-directory
      '(("HOME" . "~/")
        ("Project" . "~/MyProject/")
        ("Config" . "~/.emacs.d/")))

(provide 'init-blink-search)
