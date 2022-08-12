;;; crefactor.el --- c++ refactor                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: c++


;;; Commentary:
;;; 基于tree-sitter的c++的类refactor

;;; Code:

(require 'tree-sitter)
(require 'cl-lib)

(defun get-root-node-info (num)
  (interactive "nNumber:")
  (let ((root-node (tsc-root-node tree-sitter-tree))
        (i 0))
    (message "count: %d" (tsc-count-children root-node))
    (message "%s"
             (tsc-node-type
              (tsc-get-nth-child (tsc-get-nth-child (tsc-get-nth-child root-node 0) 6)
                                 num)))))

(cl-defun get-c++-class (&optional (node 1))
  (let ((class-node (tsc-get-parent
                     (if (equal node 1)
                         (tree-sitter-node-at-pos)
                       node))))
    (if (equal 'class_specifier
               (tsc-node-type class-node))
        class-node
      (get-c++-class class-node))))

(defun get-c++-class-funs-node (class-node)
  (let ((n (tsc-count-children class-node)))
      (if (< n 4)
          (tsc-get-nth-child class-node 2)
        (tsc-get-nth-child class-node 3))))

(defun get-c++-class-name (class-node)
  (when class-node
      (tsc-node-text
       (tsc-get-nth-child class-node 1))))

(defun get-class-name ()
  (interactive)
  (let ((class-node (get-c++-class)))
    (if class-node
        (message "%s"
                 (tsc-node-text
                  (tsc-get-nth-child class-node
                                     1)))
      (message "no have class"))))

(defun c++-class-functionp (node)
  (when (equal 'field_declaration
               (tsc-node-type node))
    (let ((child-p nil)
          (i 0)
          (n (tsc-count-children node)))
      (while (and (< i n)
                  (not child-p))
        (let ((child-node-type (tsc-node-type
                                (tsc-get-nth-child node i))))
          (if (equal 'function_declarator
                     child-node-type)
              (setq child-p t)
            (when (and (or (equal 'pointer_declarator
                                  child-node-type)
                           (equal 'reference_declarator
                                  child-node-type))
                       (> 1
                          (length
                           (get-named-child-node
                            (tsc-get-nth-child node i)))))
              (setq child-p t))))
        (setq i (+ i 1)))
      child-p)))

(defun c++-class-function-need (node)
  (let ((type (tsc-node-type node)))
    (or (equal 'type_identifier
               type)
        (equal 'pointer_declarator
               type)
        (equal 'reference_declarator
               type)
        (equal 'primitive_type
               type)
        (equal 'function_declarator
               type))))

(defun list-child-node (node)
  (let ((nodes nil))
    (dotimes (i (tsc-count-children node))
      (let ((child-node (tsc-get-nth-child node i)))
        (setq nodes
              (append nodes
                      (list
                       (list
                        (tsc-node-text child-node)
                        (tsc-node-type child-node)))))))
    nodes))

(defun get-child-nodes (node)
  (let ((nodes nil))
    (dotimes (i (tsc-count-children node))
      (setq nodes
            (append nodes
                    (list (tsc-get-nth-child node i)))))
    nodes))

(defun get-named-child-node (node)
  (let ((nodes nil))
    (dotimes (i (tsc-count-named-children node))
      (setq nodes
            (append nodes
                    (list (tsc-get-nth-named-child node i)))))
    nodes))

(cl-defun pop-list (lst &optional (n 1))
  (let ((result nil))
    (dotimes (i (- (length lst)
                   n))
      (setq result
            (append result
                    (list (elt lst i)))))
    result))

(cl-defun pop-list-front (lst &optional (n 1))
  (if (= n 0)
      lst
    (pop-list-front (cdr lst)
                    (- n 1))))

(defun last-1 (lst)
  (car (last lst)))

(defun append-1 (lst item)
  (append lst
          (list item)))

(defun get-c++-class-functions (class-node)
  (cl-remove-if-not #'(lambda (node)
                        (c++-class-functionp node))
                    (get-child-nodes
                     (get-c++-class-funs-node
                      class-node))))

(defun get-class-funs-d ()
  (interactive)
  (message "%s"
           (mapcar #'(lambda (node)
                       (list-child-node node))
                   (get-c++-class-functions
                    (get-c++-class)))))

(cl-defun get-now-node (&optional (node 1))
  (if (equal node 1)
      (let ((now-node (tree-sitter-node-at-pos)))
        (if (c++-class-functionp now-node)
            now-node
          (get-now-node (tsc-get-parent now-node))))
    (when (not (tsc-node-eq node (tsc-root-node tree-sitter-tree)))
      (if (c++-class-functionp node)
          node
        (get-now-node (tsc-get-parent node))))))

(defun handle-type (node type)
  (concat (tsc-node-text node)
          (cond ((equal 'pointer_declarator
                        type)
                 " *")
                ((equal 'reference_declarator
                        type)
                 " &")
                (t ""))))

(defun handle-name-parm (node)
  (if (equal 'array_declarator
             (tsc-node-type node))
      node
    (if (= (tsc-count-named-children node)
           0)
        node
      (let ((node-list (cl-remove-if #'(lambda (child-node)
                                         (equal (tsc-node-type child-node)
                                                'virtual_specifier))
                                     (get-named-child-node node))))
        (if (= (length node-list)
               1)
            (if (= (tsc-count-named-children (car node-list))
                   0)
                (car node-list)
              (get-child-nodes
               (car node-list)))
          node-list)))))

(cl-defun handle-type-name-l (nodes &optional (i 0) (result nil))
  (if (= (- (length nodes)
            1)
         i)
      result
    (let ((child-node (elt nodes (+ i 1)))
          (node-type (tsc-node-type (elt nodes (+ i 1)))))
      (if (cl-find node-type
                   (list 'pointer_declarator
                         'reference_declarator
                         'identifier
                         'function_declarator
                         'array_declarator)
                   :test #'equal)
          (list (append-1 result
                          (handle-type (elt nodes i)
                                       node-type))
                (handle-name-parm child-node))
        (handle-type-name-l nodes
                            (+ i 1)
                            (if (equal 'virtual_function_specifier
                                       (tsc-node-type (elt nodes i)))
                                result
                              (append-1 result
                                        (tsc-node-text
                                         (elt nodes i)))))))))

(defun handle-type-name (node)
  (handle-type-name-l (get-child-nodes node)))

(defun handle-function-name-parms (node)
  (string-join (mapcar #'(lambda (parm)
                           (let ((res (handle-type-name parm)))
                             (concat (string-join (car res)
                                                  " ")
                                     " "
                                     (tsc-node-text (cl-second res)))))
                       (get-named-child-node node))
               ", "))

(defun generate-function-implement (node)
  (let ((result (handle-type-name node)))
    (concat (last-1 (car result))
            " "
            (get-class-name)
            "::"
            (tsc-node-text (car (cl-second result)))
            "("
            (handle-function-name-parms
             (cl-second
              (cl-second result)))
            ")")))

(defun test-handle-fun ()
  (interactive)
  (message "%s"
           (mapcar #'(lambda (node)
                       (get-named-child-node node))
                   (get-child-nodes
                       (get-now-node)))))

(defun test-generate-funciotn ()
  (interactive)
  (message "%s"
           (generate-function-implement
            (get-now-node))))

(defun list-function-defination ()
  (let ((result nil)
        (root-node (tsc-root-node tree-sitter-tree)))
    (dotimes (i (tsc-count-children root-node))
      (let ((child-node (tsc-get-nth-child root-node i)))
        (if (equal #'function_definition
                   (tsc-node-type child-node))
            (setq result
                  (append-1 result
                            child-node)))))
    result))

(defun convert-function-def-stand-param (nodes)
  (concat (tsc-node-text
           (car nodes))
          "("
          (handle-function-name-parms
           (cl-second nodes))
          ")"))

(defun convert-function-def-stand (node)
  (let ((res (handle-type-name node)))
    (concat (last-1 (car res))
            " "
            (tsc-node-text (car (cl-second res)))
            "("
            (handle-function-name-parms
             (cl-second
              (cl-second res)))
            ")")))

(cl-defun get-previous-nodes (funs function-implement &optional (pre-nodes nil))
  (when funs
    (if (string= (car funs)
                 function-implement)
        pre-nodes
      (get-previous-nodes (cdr funs)
                          function-implement
                          (push (car funs)
                                pre-nodes)))))

(defun find-fun-def-node-pos-l (nodes previous-nodes)
  (when previous-nodes
    (let ((pos (cl-position (car previous-nodes)
                            nodes
                            :test #'string=)))
      (if pos
          pos
        (find-fun-def-node-pos-l nodes
                                 (cdr previous-nodes))))))

(defun find-fun-def-node-pos (nodes previous-nodes)
  "Find function defination nodes position."
  (let ((res (find-fun-def-node-pos-l (mapcar #'convert-function-def-stand
                                              (pop-list-front nodes
                                                              2))
                                      previous-nodes)))
    (tsc-node-end-position
     (elt nodes
          (if res
              (+ 2
                 res)
            1)))))

(defun test-list-function-defination-i ()
  "Test list function defination."
  (interactive)
  (message "%s"
           (mapcar #'(lambda (x)
                       (convert-function-def-stand x))
                   (list-function-defination)
                   )))

(defun test-get-class-function ()
  (interactive)
  (message "%s"
           (mapcar #'tsc-node-text
                   (get-c++-class-functions
                    (get-c++-class)))))

(defun test-get-previous-nodes ()
  (interactive)
  (message "hello"))

(defun crefactor-insert-implement ()
  "Insert a c++ class implement."
  (interactive)
  (let ((function-implement (generate-function-implement (get-now-node)))
        (funs (mapcar #'generate-function-implement
                      (get-c++-class-functions
                       (get-c++-class)))))
    (ff-find-other-file)
    (let ((nodes (list-function-defination)))
      (if (cl-find function-implement
                   (mapcar #'convert-function-def-stand
                           (pop-list-front nodes
                                           2))
                   :test #'string=)
          (ff-find-other-file)
        (progn
          (goto-char
           (find-fun-def-node-pos nodes
                                  (get-previous-nodes funs
                                                      function-implement)))
          (insert "\n")
          (insert "\n")
          (insert function-implement)
          (insert "\n")
          (insert "{")
          (insert "\n")
          (insert "\n")
          (insert "}")
          (forward-line -1)
          (execute-kbd-macro [?\t])
          (meow-insert))))))

(provide 'crefactor)
;;; crefactor.el ends here
