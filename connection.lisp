;;;; connection.lisp

(in-package #:cl-gp)

;;; *** selector ***

(defun make-selector (tags-list)
  (copy-list tags-list))

(defun copy-selector (selector)
  (copy-list selector))

;;; *** arrow ***

(defparameter *arrow/properties-test* #'equalp)

(defparameter *arrow/print-functions-list* nil)

(defclass object/arrow ()
  ((source-selector :accessor arrow/source-selector
                    :initarg :source-selector
                    :initform nil)
   (target-selector :accessor arrow/target-selector
                    :initarg :target-selector
                    :initform nil)
   (properties :accessor arrow/properties
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
  (print-unreadable-object (instance st)
    (with-slots (source-selector target-selector properties print-function) instance
      (let ((info (funcall print-function properties)))
        (format st (concatenate 'string
                                "ARROW"
                                (cond
                                  ((and source-selector target-selector)
                                   (format nil " SOURCE ~S TARGET ~S"
                                           source-selector target-selector))
                                  (source-selector
                                   (format nil " SOURCE ~S" source-selector))
                                  (target-selector
                                   (format nil " TARGET ~S" target-selector))
                                  (t ""))
                                (if (plusp (length info)) " " "")
                                info))))))

(defun make-arrow (&key source-selector target-selector properties
                     (addition-to-graph-fn (constantly nil))
                     (deletion-from-graph-fn (constantly nil))
                     (print-function (make-conjoint-print-function
                                      *arrow/print-functions-list*)))
  (make-instance 'object/arrow
                 :source-selector source-selector
                 :target-selector target-selector
                 :properties properties
                 :addition-to-graph-fn addition-to-graph-fn
                 :deletion-from-graph-fn deletion-from-graph-fn
                 :print-function print-function))

(defun copy-arrow (arrow)
  (make-arrow :source-selector (copy-selector (arrow/source-selector arrow))
              :target-selector (copy-selector (arrow/target-selector arrow))
              :properties (copy-properties (arrow/properties arrow))
              :addition-to-graph-fn (arrow/addition-to-graph-event-handler arrow)
              :deletion-from-graph-fn (arrow/deletion-from-graph-event-handler arrow)
              :print-function (arrow/print-function arrow)))

(defun arrow-equal (arrow1 arrow2)
  (funcall *arrow/properties-test* (arrow/properties arrow1) (arrow/properties arrow2)))

;;; *** connection ***

(defclass object/connection ()
  ((arrow :accessor connection/arrow
          :initarg :arrow
          :initform (error "CONNECTION -- :arrow parameter must be supplied"))
   (source :accessor connection/source-label
           :initarg :source
           :initform (error "CONNECTION -- :source parameter must be supplied"))
   (target :accessor connection/target-label
           :initarg :target
           :initform (error "CONNECTION -- :target parameter must be supplied"))))

(defmethod print-object ((instance object/connection) st)
  (print-unreadable-object (instance st)
    (with-slots (arrow source target) instance
      (format st (format nil "CONNECTION [~S]->[~S] ~S" source target arrow)))))

(defun make-connection (arrow source-label target-label)
  (make-instance 'object/connection
                 :arrow arrow
                 :source source-label
                 :target target-label))

(defun copy-connection (connection)
  (make-connection
   (copy-arrow (connection/arrow connection))
   (connection/source-label connection)
   (connection/target-label connection)))

(defun connection-equal (conn1 conn2)
  (and (label-equal (connection/source-label conn1)
                  (connection/source-label conn2))
     (label-equal (connection/target-label conn1)
                  (connection/target-label conn2))
     (arrow-equal (connection/arrow conn1)
                  (connection/arrow conn2))))

(defun connection/direction (connection label)
  (let* ((src-label (connection/source-label connection))
         (src-label-equal (label-equal label src-label))
         (tgt-label (connection/target-label connection))
         (tgt-label-equal (label-equal label tgt-label)))
    (cond
      ((and src-label-equal tgt-label-equal) (values :loop label))
      (tgt-label-equal (values :input src-label))
      (src-label-equal (values :output tgt-label))
      (t (values nil nil)))))

(defun direction/loop? (dir)
  (eql dir :loop))

(defun direction/input? (dir)
  (eql dir :input))

(defun direction/output? (dir)
  (eql dir :output))
