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
    :initform nil)
   (node-init-args
    :accessor structural-constraint/node-init-key-arguments
    :initarg :node-init-args
    :initform nil)
   (node-properties-constr-fn
    :accessor structural-constraint/node-properties-constructor-function
    :initarg :node-properties-constr-fn
    :initform nil)
   (connection-init-args
    :accessor structural-constraint/connection-init-key-arguments
    :initarg :connection-init-args
    :initform nil)
   (connection-properties-constr-fn
    :accessor structural-constraint/connection-properties-constructor-function
    :initarg :connection-properties-constr-fn
    :initform nil)
   (graph-init-args
    :accessor structural-constraint/graph-init-key-arguments
    :initarg :graph-init-args
    :initform nil)
   (graph-properties-constr-fn
    :accessor structural-constraint/graph-properties-constructor-function
    :initarg :graph-properties-constr-fn
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
   (structural-constraint/graph-event-handler-function constraint)
   :node-init-args
   (copy-list (structural-constraint/node-init-key-arguments constraint))
   :node-properties-constr-fn
   (structural-constraint/node-properties-constructor-function constraint)
   :connection-init-args
   (copy-list (structural-constraint/connection-init-key-arguments constraint))
   :connection-properties-constr-fn
   (structural-constraint/connection-properties-constructor-function constraint)
   :graph-init-args
   (copy-list (structural-constraint/graph-init-key-arguments constraint))
   :graph-properties-constr-fn
   (structural-constraint/graph-properties-constructor-function constraint)))



(defun make-conjoint-constraint-test-function (functions-list)
  (let ((functions-list (remove nil functions-list)))
    (if functions-list
        (apply #'alexandria:conjoin functions-list)
        (constantly t))))

(defun make-conjoint-event-handler-function (event-handlers-list)
  (let ((event-handlers-list (remove nil event-handlers-list)))
    (if (null event-handlers-list)
        (constantly nil)
        #'(lambda (&rest args)
            (dolist (fn event-handlers-list)
              (apply fn args))
            nil))))

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

(defmethod initialize-instance :after ((instance object/info-string-functions-package)
                                       &key common-info-string-fn)
  (when common-info-string-fn
    (with-slots (node-info-string-fn connection-info-string-fn graph-info-string-fn)
        instance
      (unless node-info-string-fn
        (setf node-info-string-fn common-info-string-fn))
      (unless connection-info-string-fn
        (setf connection-info-string-fn common-info-string-fn))
      (unless graph-info-string-fn
        (setf graph-info-string-fn common-info-string-fn)))))

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



(defun make-conjoint-info-string-function (info-functions-list)
  (let ((info-functions-list (remove nil info-functions-list)))
    (if (null info-functions-list)
        (constantly "")
        #'(lambda (&rest args)
            (reduce #'(lambda (str1 str2)
                        (concatenate 'string str1 " " str2))
                    (mapcar #'(lambda (fn)
                                (apply fn args))
                            info-functions-list))))))

;;; *** miscellaneous ***

(defmacro ~object-init-args-handling-let (args (properties-constr-fn
                                                init-args
                                                event-handler-fn
                                                info-string-fn)
                                          &body body)
  (alexandria:with-gensyms (info-string-functions-packages
                            custom-info-string-fn-first
                            structural-constraints)
    `(let* ((,info-string-functions-packages
             (getf ,args :info-string-functions-packages *info-string-functions-packages*))
            (,custom-info-string-fn-first (getf ,args :custom-info-string-fn-first))
            (,structural-constraints
             (getf ,args :structural-constraints *structural-constraints*))
            (,args (alexandria:delete-from-plist ,args
                                                 :info-string-functions-packages
                                                 :custom-info-string-fn-first
                                                 :structural-constraints))
            (,args (nconc (list (gensym) nil) ,args))
            (properties-container
             (adjoin-properties
              (nconc (alexandria:ensure-list (getf ,args :properties))
                     (mapcar
                      #'(lambda (constraint)
                          (apply (,properties-constr-fn constraint)
                                 (iterate:iter
                                   (with keys = (,init-args constraint))
                                   (for tail initially ,args then (cddr tail))
                                   (while (cddr tail))
                                   (when (member (caddr tail) keys)
                                     (nconcing (list (caddr tail) (cadddr tail)))
                                     (setf (cddr tail) (cddddr tail))))))
                      ,structural-constraints))))
            (,args (cddr ,args))
            (event-handler-function
             (make-conjoint-event-handler-function
              (nconc (mapcar (function ,event-handler-fn)
                             ,structural-constraints)
                     (list (getf ,args :event-handler-fn)))))
            (info-string-function
             (make-conjoint-info-string-function
              (let ((custom-fn (getf ,args :info-string-fn))
                    (fn-list (mapcar (function ,info-string-fn)
                                     ,info-string-functions-packages)))
                (if ,custom-info-string-fn-first
                    (cons custom-fn fn-list)
                    (nconc fn-list (list custom-fn)))))))
       ,@body)))
