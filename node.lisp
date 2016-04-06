;;;; node.lisp

(in-package #:cl-gp)

(defparameter *label-test* #'eql)

(defun label-equal (label1 label2)
  (funcall *label-test* label1 label2))

(defparameter *purpose-test* #'eql)

(defun purpose-equal (purpose1 purpose2)
  (funcall *purpose-test* purpose1 purpose2))

(defconstant +purpose/regular+ :regular)



(defclass object/node ()
  ((label :reader node/label
          :initarg :label
          :initform (error "NODE -- :label parameter must be supplied"))
   (groups :accessor node/groups
           :initarg :groups
           :initform nil)
   (purpose :reader node/purpose
            :initarg :purpose
            :initform +purpose/regular+)
   (properties :reader node/properties
               :initarg :properties
               :initform nil)
   (events-handler-fn :accessor node/events-handler-function
                      :initarg :events-handler-fn
                      :initform (constantly nil))
   (info-string-fn :accessor node/info-string-function
                   :initarg :info-string-fn
                   :initform (constantly ""))))

(defun node/description-string (node &key no-object-class)
  (let ((descr (let ((*print-circle* nil))
                 (with-slots (label purpose properties info-string-fn) node
                   (let ((info (funcall info-string-fn node)))
                     (concatenate 'string
                                  (format nil "~S [~S]" purpose label)
                                  (if (plusp (length info)) " " "")
                                  info))))))
    (if no-object-class
        descr
        (concatenate 'string "NODE " descr))))

(defmethod print-object ((instance object/node) st)
  (print-unreadable-object (instance st)
    (format st (node/description-string instance))))

(defun node/regular? (node)
  (purpose-equal (node/purpose node) +purpose/regular+))

(defun make-node (label &rest args)
  (apply (alexandria:curry #'make-instance 'object/node
                           :label label) args))

(defun copy-node (node)
  (make-node
   (node/label node)
   :purpose (node/purpose node)
   :properties (copy-properties (node/properties node))
   :events-handler-fn (node/events-handler-function node)
   :info-string-fn (node/info-string-function node)))
