;;;; node.lisp

(in-package #:cl-gp)

;;; *** node ***

(defparameter *node/id-test* #'eql)

(defun id-equal (id1 id2)
  (funcall *node/id-test* id1 id2))

(defparameter *node/print-functions-list* nil)

(defclass object/node ()
  ((id :accessor node/id
       :initarg :id
       :initform (error "NODE -- :id parameter must be supplied"))
   (properties :accessor node/properties
               :initarg :properties
               :initform nil)
   (addition-to-graph-fn :accessor node/addition-to-graph-event-handler
                         :initarg :addition-to-graph-fn
                         :initform (constantly nil))
   (deletion-from-graph-fn :accessor node/deletion-from-graph-event-handler
                           :initarg :deletion-from-graph-fn
                           :initform (constantly nil))
   (setting-of-connection-fn :accessor node/setting-of-connection-event-handler
                             :initarg :setting-of-connection-fn
                             :initform (constantly nil))
   (loss-of-connection-fn :accessor node/loss-of-connection-event-handler
                          :initarg :loss-of-connection-fn
                          :initform (constantly nil))
   (print-function :accessor node/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/node) st)
  (print-unreadable-object (instance st)
    (with-slots (id properties print-function) instance
      (let ((info (funcall print-function properties)))
        (format st (concatenate 'string
                                (format nil "NODE[~S]" id)
                                (if (plusp (length info)) " " "")
                                info))))))

(defun make-node (id &key properties
                       (addition-to-graph-fn (constantly nil))
                       (deletion-from-graph-fn (constantly nil))
                       (setting-of-connection-fn (constantly nil))
                       (loss-of-connection-fn (constantly nil))
                       (print-function (make-conjoint-print-function
                                        *node/print-functions-list*)))
  (make-instance 'object/node
                 :id id
                 :properties properties
                 :addition-to-graph-fn addition-to-graph-fn
                 :deletion-from-graph-fn deletion-from-graph-fn
                 :setting-of-connection-fn setting-of-connection-fn
                 :loss-of-connection-fn loss-of-connection-fn
                 :print-function print-function))

(defun copy-node (node)
  (make-node (node/id node)
             :properties (copy-properties (node/properties node))
             :addition-to-graph-fn (node/addition-to-graph-event-handler node)
             :deletion-from-graph-fn (node/deletion-from-graph-event-handler node)
             :setting-of-connection-fn (node/setting-of-connection-event-handler node)
             :loss-of-connection-fn (node/loss-of-connection-event-handler node)
             :print-function (node/print-function node)))
