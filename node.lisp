;;;; node.lisp

(in-package #:cl-gp)

;;; *** typed name ***

(defun typed-name/new (name typ)
  (list name 'as typ))

(defun typed-name/name (tname)
  (first tname))
(defun (setf typed-name/name) (new-value tname)
  (setf (first tname) new-value))

(defun typed-name/type (tname)
  (third tname))
(defun (setf typed-name/type) (new-value tname)
  (setf (third tname) new-value))

;;; *** callee kinds ***

(defconstant +callee/value+ :value)
(defconstant +callee/primitive+ :primitive)
(defconstant +callee/module+ :module)
(defconstant +callee/input+ :input)
(defconstant +callee/output+ :output)

;;; *** node ***

(defun node/new (callee-kind callee-name &key inputs outputs properties)
  (cond
    ((and (eql callee-kind +callee/value+)
        (not (and (null inputs)
              (> (length outputs) 0))))
     (error "NODE/NEW -- (value) incorrect node pins configuration"))
    ((and (eql callee-kind +callee/input+)
        (not (and (null inputs)
              (> (length outputs) 0))))
     (error "NODE/NEW -- (input) incorrect node pins configuration"))
    ((and (eql callee-kind +callee/output+)
        (not (and (null outputs)
              (> (length inputs) 0))))
     (error "NODE/NEW -- (output) incorrect node pins configuration")))
  (list callee-kind callee-name inputs outputs properties))

(defun node/callee-kind (node)
  (first node))
(defun (setf node/callee-kind) (new-value node)
  (setf (first node) new-value))

(defun node/callee-name (node)
  (second node))
(defun (setf node/callee-name) (new-value node)
  (setf (second node) new-value))

(defun node/inputs (node)
  (third node))
(defun (setf node/inputs) (new-value node)
  (setf (third node) new-value))

(defun node/outputs (node)
  (fourth node))
(defun (setf node/outputs) (new-value node)
  (setf (fourth node) new-value))

(defun node/properties (node)
  (fifth node))
(defun (setf node/properties) (new-value node)
  (setf (fifth node) new-value))

(defun node/value? (node)
  (eql (node/callee-kind node) +callee/value+))

(defun node/primitive? (node)
  (eql (node/callee-kind node) +callee/primitive+))

(defun node/module? (node)
  (eql (node/callee-kind node) +callee/module+))

(defun node/input? (node)
  (eql (node/callee-kind node) +callee/input+))

(defun node/output? (node)
  (eql (node/callee-kind node) +callee/output+))

;;; *** node type ***

(defun node/type (node)
  (cond
    ((or (node/value? node)
        (node/input? node)) (tuple/new (mapcar #'typed-name/type (node/outputs node))))
    ((or (node/primitive? node)
        (node/module? node))
     (function-type/new (tuple/new (mapcar #'typed-name/type (node/inputs node)))
                        (tuple/new (mapcar #'typed-name/type (node/outputs node)))))
    (t +type/bottom+)))
