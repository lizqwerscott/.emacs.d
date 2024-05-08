;;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4)


(setf c-ts-mode-indent-offset 4)

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".ipp" ".h"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c" ".cpp"))
    ))

(setq ff-search-directories
      '("." "../src" "../include"))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(provide 'init-c++)
