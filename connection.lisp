;;;; connection.lisp

(in-package #:cl-gp)

;;; *** arrow ***

(defparameter *arrow/test* #'equalp)

(defparameter *arrow/print-functions-list* nil)

(defclass object/arrow ()
  ((properties :reader arrow/properties
               :initarg :properties
               :initform nil)
   (print-function :accessor arrow/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/arrow) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (properties print-function) instance
      (format st "ARROW ~A" (funcall print-function properties)))))

(defun make-arrow (&key properties (print-function (make-conjoint-print-function
                                                    *arrow/print-functions-list*)))
  (make-instance 'object/arrow
                 :properties properties
                 :print-function print-function))

(defun copy-arrow (arrow)
  (make-arrow (funcall *properties-copy-function* (arrow/properties arrow))
              (arrow/print-function arrow)))

(defun arrow-equal (arrow1 arrow2)
  (funcall *arrow/test* (arrow/properties arrow1) (arrow/properties arrow)))

;;; *** connection ***

(defclass object/connection ()
  ((arrow :reader connection/arrow
          :initarg :arrow
          :initform (error "OBJECT/CONNECTION -- :arrow parameter must be supplied"))
   (source :reader connection/source-id
           :initarg :source
           :initform (error "OBJECT/CONNECTION -- :source parameter must be supplied"))
   (target :reader connection/target-id
           :initarg :target
           :initform (error "OBJECT/CONNECTION -- :target parameter must be supplied"))))

(defmethod print-object ((instance object/connection) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (arrow source target) instance
      (format st "CONNECTION (~S -> ~S) ~S" source target arrow))))

(defun make-connection (arrow source-id target-id)
  (make-instance 'object/connection
                 :arrow arrow
                 :source source-id
                 :target target-id))

(defun copy-connection (connection)
  (make-connection
   (copy-arrow (connection/arrow connection))
   (connection/source-id connection)
   (connection/target-id connection)))

(defun connection-equal (conn1 conn2)
  (and (id-equal (connection/source-id conn1)
               (connection/source-id conn2))
     (id-equal (connection/target-id conn1)
               (connection/target-id conn2))
     (arrow-equal (connection/arrow conn1)
                  (connection/arrow conn2))))
