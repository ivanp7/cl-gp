;;;; node.lisp

(in-package #:cl-gp)

;;; *** node ***

(defparameter *node/id-test* #'eql)

(defun id-equal (id1 id2)
  (funcall *node/id-test* id1 id2))

(defclass genotype/node ()
  ((id :reader node/id
       :initarg :id)
   (properties :reader node/properties
               :initarg :properties
               :initform nil)
   (print-function :accessor node/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance genotype/node) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (id properties print-function) instance
      (format st "NODE#~S ~A" id (funcall print-function properties)))))

(defun node-object? (object)
  (typep object 'genotype/node))

(defun make-genotype-node (id &key properties (print-function (constantly "")))
  (make-instance 'genotype/node
                 :id id :properties properties
                 :print-function print-function))

(defun copy-genotype-node (node)
  (make-genotype-node (node/id node)
                      :properties (funcall *properties-copy-function* (node/properties node))
                      :print-function (node/print-function node)))

;;; *** standard node features ***
#|
(defconstant +behaviour/value+ nil)
(defconstant +behaviour/function-call+ :function)

(defun make-node-properties (&key name behaviour (type +type/bottom+) object)
  (case behaviour
    (+behaviour/function-call+
     (unless (type/function? type)
       (error "GENOTYPE/NODE -- 'function call' node must have function type")))
    (+behaviour/value+ nil)
    (t (error "GENOTYPE/NODE -- invalid behaviour is specified")))
  (list :name name
        :behaviour behaviour
        :type type
        :object object))

(defun node/name (node)
  (getf (node/properties node) :name))

(defun node/behaviour (node)
  (getf (node/properties node) :behaviour))

(defun node/object (node)
  (getf (node/properties node) :object))





(defun node/value? (node)
  (eql (node/behaviour node) +behaviour/value+))

(defun node/call? (node)
  (eql (node/behaviour node) +behaviour/function-call+))



(defun copy-node (node &optional (new-id (node/id node)))
  (make-node new-id (copy-tree (node/properties node))))
|#
