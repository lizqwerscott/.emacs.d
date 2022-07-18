;;; crefactor.el --- c++ refactor                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>
;; Keywords: c


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
                         (tree-sitter-node-at-point)
                       node))))
    (if (equal 'class_specifier
               (tsc-node-type class-node))
        class-node
      (get-c++-class class-node))))

(defun get-c++-class-funs-node ()
  (let ((class-node (get-c++-class)))
    (let ((n (tsc-count-children class-node)))
      (if (< n 4)
          (tsc-get-nth-child class-node 2)
        (tsc-get-nth-child class-node 3)))))

(defun get-c++-class-name ()
  (let ((class-node (get-c++-class)))
    (when class-node
      (tsc-node-text
       (tsc-get-nth-child class-node 1)))))

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
          (message "node-type: %s" child-node-type)
          (if (or (equal 'pointer_declarator
                         child-node-type)
                  (equal 'reference_declarator
                         child-node-type)
                  (equal 'function_declarator
                         child-node-type))
              (setq child-p t)))
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

;; TODO 需要简化
(defun convert-class-function-node (node)
  (let ((nodes nil))
    (dotimes (i (tsc-count-named-children node))
      (let ((child-node (tsc-get-nth-named-child node i)))
        (when (c++-class-function-need child-node)
          (setq nodes
                (append nodes
                        (list child-node))))))
    nodes))

(defun get-class-functions ()
  (let ((class-node (get-c++-class-funs-node))
        (funs nil))
    (dotimes (i (tsc-count-children class-node))
      (let ((node (tsc-get-nth-child class-node
                                     i)))
        (when (c++-class-functionp node)
          (setf funs
                (append funs
                        (list node))))))
    funs))

(defun get-c++-class-functions ()
  (cl-remove-if-not #'(lambda (node)
                        (c++-class-functionp node))
                    (get-child-nodes
                     (get-c++-class-funs-node))))

(defun get-class-funs-d ()
  (interactive)
  (message "%s"
           (mapcar #'(lambda (node)
                       (list-child-node node))
                   (get-c++-class-functions))))

(defun get-class-funs ()
  (interactive)
  (message "%s"
           (tsc-node-text
            (get-c++-class-funs-node))))

(cl-defun get-now-node (&optional (node 1))
  (if (equal node 1)
      (let ((now-node (tree-sitter-node-at-point)))
        (if (c++-class-functionp now-node)
            now-node
          (get-now-node (tsc-get-parent now-node))))
    (when (not (tsc-node-eq node (tsc-root-node tree-sitter-tree)))
      (if (c++-class-functionp node)
          node
        (get-now-node (tsc-get-parent node))))))

;; TODO 需要在实现列表里面找到这个前一个节点，才能插入
(defun get-previous-node (now-node)
  (let ((funs (get-c++-class-functions))
        (i 0)
        (result nil))
    (while (and (< i (length funs))
                (not result))
      (when (tsc-node-eq now-node
                         (elt funs i))
        (setq result (- i 1)))
      (setq i (+ i 1)))
    (when (>= result 0)
      (elt funs result))))

(defun is-return-pointer-function (node)
  (let ((isp nil)
        (i 0))
    (while (and (< i (length node))
                (not isp))
      (when (equal 'pointer_declarator
                    (tsc-node-type (elt node i)))
        (setq isp t))
      (setq i (+ i 1)))
    isp))

(defun get-function-return-type (node)
  (concat (tsc-node-text (car node))
          (if (is-return-pointer-function node)
              " * "
            "")))

(defun append-1 (lst item)
  (append lst
          (list item)))

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
        node-list))))

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
                         'function_declarator)
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

(defun generate-function-implement-tt ()
  (let ((result (handle-type-name (get-now-node))))
    (concat (string-join (car result)
                         " ")
            " "
            (get-class-name)
            "::"
            (tsc-node-text (car (cl-second result)))
            "("
            (string-join (mapcar #'(lambda (parm)
                                     (let ((res (handle-type-name parm)))
                                       (concat (string-join (car res)
                                                            " ")
                                               " "
                                               (tsc-node-text (cl-second res)))))
                                 (get-named-child-node
                                  (cl-second
                                   (cl-second result))))
                         ", ")
            ")")))

(defun test-handle-type-name ()
  (interactive)
  (message "%s"
           (mapcar #'(lambda (node)
                       (handle-type-name node))
                   (get-named-child-node (tsc-get-nth-child (tsc-get-nth-child (elt (convert-class-function-node
                                                                                     (get-now-node))
                                                                                    1)
                                                                               1)
                                                            1)))))

(defun test-handle-fun ()
  (interactive)
  (message "%s"
           (handle-type-name
            (get-now-node))))

(defun test-generate-funciotn-tt ()
  (interactive)
  (message "%s"
           (generate-function-implement-tt)))

(defun test-child-node ()
  (interactive)
  (message "%s"
           (list-child-node (tsc-get-nth-child (get-now-node)
                                               1))))

(defun handle-optional-function (nodes)
  (let ((result nil))
    (dotimes (i (tsc-count-named-children nodes))
      (let ((child-node (tsc-get-nth-named-child nodes i)))
        (if (equal 'optional_parameter_declaration
                   (tsc-node-type child-node))
            (setq result
                  (append result
                          (list
                           (s-join " "
                                   (mapcar #'tsc-node-text
                                           (pop-list (get-child-nodes child-node)
                                                     2))))))
          (setq result
                (append result
                        (list (tsc-node-text child-node)))))))
    (s-join ", "
            result)))

(defun get-function-name-and-params (node)
  (let ((body (car (cdr node))))
    (concat (tsc-node-text (tsc-get-nth-named-child body 0))
            "("
            ;; (handle-optional-function (tsc-get-nth-named-child body 1))
            ")")))

(defun generate-function-implement-l ()
  (let ((now-node (convert-class-function-node (get-now-node))))
    (concat (get-function-return-type now-node)
            " "
            (get-c++-class-name)
            "::"
            (get-function-name-and-params now-node))))

(defun test-generate-funciotn ()
  (interactive)
  (message "%s"
           (generate-function-implement)))

;; 1. 生成implement
;; 2. 首先简单的插入最后
;; TODO: 排序插入
(defun insert-implement ()
  (interactive)
  (let ((function-implement (generate-function-implement-tt)))
    (ff-find-other-file)
    (goto-char (point-max))
    (insert function-implement)
    (insert "\n")
    (insert "{")
    (insert "\n")
    (insert "\n")
    (insert "}")
    (previous-line)
    (execute-kbd-macro [?\t])
    (meow-insert)))

;; ＃ 除了构造函数，每个函数由三部分组成
;; 1. 函数返回值
;; 2. 函数实现
;; 3. 函数体
;; 函数定义部分，生成函数实现，看是否相等。
;; 返回值，函数名字，参数列表，若是这个要插入的函数的前一个，着在函数体后插入。
;;
;; ＃ 如何获得当前函数的前一个函数的信息。
(defun list-function-defination ()
  (let ((result nil)
        (root-node (tsc-root-node tree-sitter-tree)))
    (dotimes (i (tsc-count-children root-node))
      (let ((child-node (tsc-get-nth-child root-node i)))
        (if (equal #'function_definition
                   (tsc-node-type child-node))
            (setq result
                  (append result
                          (list
                           (pop-list (get-child-nodes child-node)
                                     1)))))))
    result))

(defun convert-function-def-stand-param (nodes)
  (concat (tsc-node-text
           (car nodes))
          "("
          (handle-optional-function (elt nodes 1))
          ")"))

(defun convert-function-def-stand (node)
  (concat (tsc-node-text
           (car node))
          " "
          (convert-function-def-stand-param
           (get-child-nodes
            (elt node 1)))))

(defun find-function-defination (nodes now-node)
  (let ((result nil)
        (i 0))
    (while (and (< i (length nodes))
                (not result))
      (let ((node (elt nodes i)))
        (when ()))
      (setq i (+ i 1)))))

(defun list-function-defination-i ()
  (interactive)
  (message "%s"
           (mapcar #'convert-function-def-stand
                   (pop-list-front (list-function-defination)
                                   2))))

(provide 'crefactor)
;;; crefactor.el ends here
