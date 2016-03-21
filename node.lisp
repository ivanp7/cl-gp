;;;; node.lisp

(in-package #:cl-gp)

;;; *** node ***

(defparameter *node/id-test* #'eql)

(defun id-equal (id1 id2)
  (funcall *node/id-test* id1 id2))

(defparameter *node/print-functions-list* nil)

(defclass object/node ()
  ((id :reader node/id
       :initarg :id)
   (properties :reader node/properties
               :initarg :properties
               :initform nil)
   (print-function :accessor node/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/node) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (id properties print-function) instance
      (format st "NODE#~S ~A" id (funcall print-function properties)))))

(defun make-node (id &key properties (print-function (make-conjoint-print-function
                                                      *node/print-functions-list*)))
  (make-instance 'object/node
                 :id id :properties properties
                 :print-function print-function))

(defun copy-node (node)
  (make-node (node/id node)
             :properties (funcall *properties-copy-function* (node/properties node))
             :print-function (node/print-function node)))
