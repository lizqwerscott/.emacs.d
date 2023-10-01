;;;###autoload
(defmacro add-hooks (modes fn)
  `(dolist (mode ,modes)
     (add-hook (intern
                (concat (symbol-name mode)
                        "-hook"))
               ,fn)))

;;;###autoload
(defun add-list-to-list (list-var elements)
  (if (listp elements)
      (mapcar #'(lambda (element)
                  (add-to-list list-var element))
              (reverse elements))
    (add-to-list list-var elements)))

(provide 'init-utils)
