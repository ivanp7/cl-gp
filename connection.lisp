;;;; connection.lisp

(in-package #:cl-gp)

;;; *** arrow ***

(defparameter *arrow/properties-test* #'equalp)

(defparameter *arrow/print-functions-list* nil)

(defclass object/arrow ()
  ((properties :reader arrow/properties
               :initarg :properties
               :initform nil)
   (addition-to-graph-fn :accessor arrow/addition-to-graph-event-handler
                         :initarg :addition-to-graph-fn
                         :initform (constantly nil))
   (deletion-from-graph-fn :accessor arrow/deletion-from-graph-event-handler
                           :initarg :deletion-from-graph-fn
                           :initform (constantly nil))
   (print-function :accessor arrow/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/arrow) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (properties print-function) instance
      (format st "ARROW ~A" (funcall print-function properties)))))

(defun make-arrow (&key properties
                     (addition-to-graph-fn (constantly nil))
                     (deletion-from-graph-fn (constantly nil))
                     (print-function (make-conjoint-print-function
                                      *arrow/print-functions-list*)))
  (make-instance 'object/arrow
                 :properties properties
                 :addition-to-graph-fn addition-to-graph-fn
                 :deletion-from-graph-fn deletion-from-graph-fn
                 :print-function print-function))

(defun copy-arrow (arrow)
  (make-arrow :properties (funcall *properties-copy-function* (arrow/properties arrow))
              :addition-to-graph-fn (arrow/addition-to-graph-event-handler arrow)
              :deletion-from-graph-fn (arrow/deletion-from-graph-event-handler arrow)
              :print-function (arrow/print-function arrow)))

(defun arrow-equal (arrow1 arrow2)
  (funcall *arrow/properties-test* (arrow/properties arrow1) (arrow/properties arrow)))

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

(defun connection/other-id (conn id)
  (let ((s-id (connection/source-id conn))
        (s-id-equal (id-equal id s-id))
        (t-id (connection/target-id conn))
        (t-id-equal (id-equal id t-id)))
    (cond
      ((and s-id-equal t-id-equal) (values id :loop))
      (t-id-equal (values s-id :input))
      (s-id-equal (values t-id :output))
      (t (values nil nil)))))
