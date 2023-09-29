;;;###autoload
(defmacro add-hooks (modes fn)
  `(dolist (mode ,modes)
     (add-hook (intern
                (concat (symbol-name mode)
                        "-hook"))
               ,fn)))

(provide 'init-utils)
