;;;; node.lisp

(in-package #:cl-gp)

(defparameter *node/label-test* #'eql)

(defun label-equal (label1 label2)
  (funcall *node/label-test* label1 label2))

(defparameter *node/print-functions-list* nil)



(defclass object/node ()
  ((label :accessor node/label
          :initarg :label
          :initform (error "NODE -- :label parameter must be supplied"))
   (properties :accessor node/properties
               :initarg :properties
               :initform nil)
   (module :reader node/associated-module
           :initform nil)
   (events-handler-fn :accessor node/events-handler-function
                      :initarg :events-handler-fn
                      :initform (constantly nil))
   (print-function :accessor node/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defun node/primitive? (node)
  (null (node/associated-module node)))

(defun node/module-associated? (node)
  (not (node/primitive? node)))

(defmethod print-object ((instance object/node) st)
  (print-unreadable-object (instance st)
    (with-slots (label properties module print-function) instance
      (let ((info (funcall print-function properties module)))
        (format st (concatenate 'string
                                (if (node/primitive? instance)
                                    "PRIMITIVE-NODE " "MODULE-NODE ")
                                (format nil "[~S]" label)
                                (if (plusp (length info)) " " "")
                                info))))))

(defun make-node (label &key properties
                          (events-handler-fn (constantly nil))
                          (print-function (make-conjoint-print-function
                                           *node/print-functions-list*)))
  (make-instance 'object/node
                 :label label
                 :properties properties
                 :events-handler-fn events-handler-fn
                 :print-function print-function))

(defun copy-primitive-node (node)
  (make-node
   (node/label node)
   :properties (copy-properties (node/properties node))
   :events-handler-fn (node/events-handler-function node)
   :print-function (node/print-function node)))

(declaim (ftype function copy-module-node))

(defun copy-node (node)
  (if (node/primitive? node)
      (copy-primitive-node node)
      (copy-module-node node)))
