;;;; node.lisp

;;; Public functions/constants/variables of the module:
#|
node/new-value
node/new-primitive
node/new-module
node/new-input
node/new-output
node/copy

node/kind
node/name
node/input-type
node/output-type
node/properties

node/value?
node/primitive?
node/module?
node/input?
node/output?

node/type
|#

(in-package #:cl-gp)

;;; *** node kinds ***

(defconstant +node/value+ :value)
(defconstant +node/primitive+ :primitive)
(defconstant +node/module+ :module)
(defconstant +node/input+ :input)
(defconstant +node/output+ :output)

;;; *** node ***

(defun node~/new (kind name &key (input-type +type/bottom+)
                              (output-type +type/bottom+)
                              properties)
  (cond
    ((and (eql kind +node/value+)
        (or (not (type/bottom? input-type))
           (type/bottom? output-type)))
     (error "NODE~~/NEW -- node of kind 'value' has incorrect input/output type"))
    ((and (eql kind +node/input+)
        (or (not (type/bottom? input-type))
           (type/bottom? output-type)))
     (error "NODE~~/NEW -- node of kind 'input' has incorrect input/output type"))
    ((and (eql kind +node/output+)
        (or (type/bottom? input-type)
           (not (type/bottom? output-type))))
     (error "NODE~~/NEW -- node of kind 'output' has incorrect input/output type")))
  (list kind name input-type output-type properties))

(defun node/new-value (name output-type &optional properties)
  (node~/new +node/value+ name
             :output-type output-type
             :properties properties))

(defun node/new-primitive (name input-type output-type &optional properties)
  (node~/new +node/primitive+ name
             :input-type input-type
             :output-type output-type
             :properties properties))

(defun node/new-module (name input-type output-type &optional properties)
  (node~/new +node/module+ name
             :input-type input-type
             :output-type output-type
             :properties properties))

(defun node/new-input (name output-type &optional properties)
  (node~/new +node/input+ name
             :output-type output-type
             :properties properties))

(defun node/new-output (name input-type &optional properties)
  (node~/new +node/output+ name
             :input-type input-type
             :properties properties))

(defun node/copy (node)
  (copy-list node))



(defun node/kind (node)
  (first node))
(defun (setf node/kind) (new-value node)
  (setf (first node) new-value))

(defun node/name (node)
  (second node))
(defun (setf node/name) (new-value node)
  (setf (second node) new-value))

(defun node/input-type (node)
  (third node))
(defun (setf node/input-type) (new-value node)
  (setf (third node) new-value))

(defun node/output-type (node)
  (fourth node))
(defun (setf node/output-type) (new-value node)
  (setf (fourth node) new-value))

(defun node/properties (node)
  (fifth node))
(defun (setf node/properties) (new-value node)
  (setf (fifth node) new-value))

(defun node/value? (node)
  (eql (node/kind node) +node/value+))

(defun node/primitive? (node)
  (eql (node/kind node) +node/primitive+))

(defun node/module? (node)
  (eql (node/kind node) +node/module+))

(defun node/input? (node)
  (eql (node/kind node) +node/input+))

(defun node/output? (node)
  (eql (node/kind node) +node/output+))

(defun node/type (node)
  (cond
    ((node/value? node) (node/output-type node))
    ((or (node/primitive? node)
        (node/module? node))
     (function-type/new (node/input-type node)
                        (node/output-type node)))
    (t +type/bottom+)))
