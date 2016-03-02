;;;; node.lisp

;;; Public functions/constants/variables of the module:
#|
node/abstract
node/name
node/id
node/module
node-object?

+purpose/value+
+purpose/call+
node/regular
node/purpose
node/object
node/object-type
node/properties
node/regular?
make-regular-node
node/callable?
node/value?
node/input-type
node/output-type
regular-node/copy

+direction/input+
+direction/output+
node/auxiliary
node/direction
node/socket-type
node/auxiliary?
make-auxiliary-node
node/input?
node/output?
auxiliary-node/copy
|#

(in-package #:cl-gp)

;;; *** abstract node ***

(defclass node/abstract ()
  ((name :reader node/name
         :initarg :name
         :initform nil)
   (id :reader node/id
       :initform nil)
   (module :reader node/module
           :initform nil)))

(defmethod initialize-instance :after ((instance node/abstract) &key)
  (if (alexandria:type= (type-of instance) 'node/abstract)
      (error "NODE/ABSTRACT -- abstract node cannot be made")))

(defmethod print-object ((instance node/abstract) st)
  (print-unreadable-object (instance st)
    (with-slots (id name) instance
      (format st "NODE/ABSTRACT: id: ~S, name: ~S" id name))))

(defun node-object? (object)
  (typep object 'node/abstract))

;;; *** regular node ***

(defconstant +purpose/value+ :value)
(defconstant +purpose/call+ :call)

(defclass node/regular (node/abstract)
  ((purpose :reader node/purpose
            :initarg :purpose
            :initform (error "NODE/REGULAR -- :purpose parameter must be supplied"))
   (object :reader node/object
           :initarg :object
           :initform nil)
   (object-type :reader node/object-type
                :initarg :type
                :initform +type/bottom+)
   (properties :reader node/properties
               :initarg :properties
               :initform nil)))

(defmethod initialize-instance :after ((instance node/regular) &key)
  (with-slots (purpose object-type) instance
    (if (typed-value-object? object)
        (case purpose
          (+purpose/call+
           (unless (type/function? object-type)
             (error "NODE/REGULAR -- 'callable' node must have an object of a function type")))
          (+purpose/value+ nil)
          (t (error "NODE/REGULAR -- invalid purpose is specified")))
        (error "NODE/REGULAR -- invalid object type is specified"))))

(defmethod print-object ((instance node/regular) st)
  (print-unreadable-object (instance st)
    (with-slots (id name purpose object-type) instance
      (format st "REGULAR-NODE: id: ~S, name: ~S, purpose: ~S, type: ~S"
              id name purpose object-type))))

(defun node/regular? (object)
  (typep object 'node/regular))

(defun make-regular-node (name callable? object object-type &optional properties)
  (make-instance 'node/regular :name name
                 :purpose (if callable? +purpose/call+ +purpose/value+)
                 :object object :object-type object-type :properties properties))

(defun node/callable? (node)
  (eql (node/purpose node) +purpose/call+))

(defun node/value? (node)
  (eql (node/purpose node) +purpose/value+))

(defun node/input-type (node)
  (if (node/callable? node)
      (function-type/argument (node/object-type node))
      +type/bottom+))

(defun node/output-type (node)
  (if (node/callable? node)
      (function-type/result (node/object-type node))
      (node/object-type node)))

(defun regular-node/copy (node)
  (make-regular-node (node/name node)
                     (node/callable? node)
                     (node/object node)
                     (node/object-type node)
                     (node/properties node)))

;;; *** auxiliary (I/O) node ***

(defconstant +direction/input+ :input)
(defconstant +direction/output+ :output)

(defclass node/auxiliary (node/abstract)
  ((direction :reader node/direction
              :initarg :direction
              :initform (error "NODE/AUXILIARY -- :direction parameter must be supplied"))
   (socket-type :reader node/socket-type
                :initarg :type
                :initform +type/bottom+)))

(defmethod initialize-instance :after ((instance node/auxiliary) &key)
  (with-slots (direction socket-type) instance
    (if (type-object? socket-type)
        (case direction
          ((+direction/input+ +direction/output+) nil)
          (t (error "NODE/AUXILIARY -- invalid socket direction is specified")))
        (error "NODE/AUXILIARY -- invalid socket type is specified"))))

(defmethod print-object ((instance node/auxiliary) st)
  (print-unreadable-object (instance st)
    (with-slots (id name direction socket-type) instance
      (format st "AUXILIARY-NODE: id: ~S, name: ~S, direction ~S, type: ~S"
              id name direction socket-type))))

(defun node/auxiliary? (object)
  (typep object 'node/auxiliary))

(defun make-auxiliary-node (name direction socket-type)
  (make-instance 'node/auxiliary :name name
                 :direction direction :socket-type socket-type))

(defun node/input? (node)
  (eql (node/direction node) +direction/input+))

(defun node/output? (node)
  (eql (node/direction node) +direction/output+))

(defun auxiliary-node/copy (node)
  (make-auxiliary-node (node/name node)
                       (node/direction node)
                       (node/socket-type node)))
