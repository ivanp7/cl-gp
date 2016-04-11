;;;; structural-constraints.lisp

(in-package #:cl-gp)

(defparameter *structural-constraints* nil)

(defclass object/structural-constraint ()
  ((name :accessor structural-constraint/name
         :initarg :name
         :initform nil)
   (constraint-test-fn
    :accessor structural-constraint/test-function
    :initarg :constraint-test-fn
    :initform nil)
   (node-event-handler-fn
    :accessor structural-constraint/node-event-handler-function
    :initarg :node-event-handler-fn
    :initform nil)
   (connection-event-handler-fn
    :accessor structural-constraint/connection-event-handler-function
    :initarg :connection-event-handler-fn
    :initform nil)
   (graph-event-handler-fn
    :accessor structural-constraint/graph-event-handler-function
    :initarg :graph-event-handler-fn
    :initform nil)))

(defmethod print-object ((instance object/structural-constraint) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "STRUCTURAL-CONSTRAINT ~S" name))))

(defun make-structural-constraint (&rest args)
  (apply (alexandria:curry #'make-instance 'object/structural-constraint) args))

(defun copy-structural-constraint (constraint)
  (make-structural-constraint
   :name (structural-constraint/name constraint)
   :constraint-test-fn
   (structural-constraint/test-function constraint)
   :node-event-handler-fn
   (structural-constraint/node-event-handler-function constraint)
   :connection-event-handler-fn
   (structural-constraint/connection-event-handler-function constraint)
   :graph-event-handler-fn
   (structural-constraint/graph-event-handler-function constraint)))



(defun make-conjoint-constraint-test-function (functions-list)
  (let ((functions-list (remove nil functions-list)))
    (if functions-list
        (apply #'alexandria:conjoin functions-list)
        (constantly t))))

(defun make-conjoint-event-handler-function (event-handlers-list)
  (let ((event-handlers-list (remove nil event-handlers-list)))
    (if event-handlers-list
        #'(lambda (&rest args)
            (dolist (fn event-handlers-list)
              (apply fn args))
            nil)
        (constantly nil))))

;;; *** info string functions package  ***

(defparameter *info-string-functions-packages* nil)

(defclass object/info-string-functions-package ()
  ((name :accessor info-string-functions-package/name
         :initarg :name
         :initform nil)
   (node-info-string-fn
    :accessor info-string-functions-package/node-info-string-function
    :initarg :node-info-string-fn
    :initform nil)
   (connection-info-string-fn
    :accessor info-string-functions-package/connection-info-string-function
    :initarg :connection-info-string-fn
    :initform nil)
   (graph-info-string-fn
    :accessor info-string-functions-package/graph-info-string-function
    :initarg :graph-info-string-fn
    :initform nil)))

(defmethod print-object ((instance object/info-string-functions-package) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "INFO-STRING-FUNCTIONS-PACKAGE ~S" name))))

(defun make-info-string-functions-package (&rest args)
  (apply (alexandria:curry #'make-instance 'object/info-string-functions-package) args))

(defun copy-info-string-functions-package (info-pkg)
  (make-info-string-functions-package
   :name (info-string-functions-package/name info-pkg)
   :node-info-string-fn
   (info-string-functions-package/node-info-string-function info-pkg)
   :connection-info-string-fn
   (info-string-functions-package/connection-info-string-function info-pkg)
   :graph-info-string-fn
   (info-string-functions-package/graph-info-string-function info-pkg)))



(defun make-conjoint-info-function (info-functions-list)
  (let ((info-functions-list (remove nil info-functions-list)))
    (if (null info-functions-list)
        (constantly "")
        #'(lambda (&rest args)
            (reduce #'(lambda (str1 str2)
                        (concatenate 'string str1 " " str2))
                    (mapcar #'(lambda (fn)
                                (apply fn args))
                            info-functions-list))))))
