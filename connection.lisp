;;;; connection.lisp

(in-package #:cl-gp)

;;; *** abstract selector ***

(defclass selector/abstract ()
  ())

;;; *** arrow ***

(defun make-arrow (source-selector target-selector)
  ...)

(defun copy-arrow (arrow)
  )

(defun arrow-equal (arrow1 arrow2)
  )

;;; *** connection ***

(defun make-connection (arrow source-id target-id)
  ...)

(defun copy-connection (connection)
  (make-connection
   (copy-arrow (connection/arrow connection))
   (connection/source-id connection)
   (connection/target-id connection)))

(defun connection-equal (conn1 conn2)
  (and (funcall *node/id-test*
              (connection/source-id conn1)
              (connection/source-id conn2))
     (funcall *node/id-test*
              (connection/target-id conn1)
              (connection/target-id conn2))
     (arrow-equal (connection/arrow conn1)
                  (connection/arrow conn2))))
