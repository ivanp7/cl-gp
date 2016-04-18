;;;; structural-constraints.lisp

(in-package #:cl-gp)

(defparameter *structural-constraints* nil)

(defclass object/structural-constraint ()
  ((name :accessor structural-constraint/name
         :initarg :name
         :initform nil)
   (info-string-getter :accessor structural-constraint/info-string-getter
                       :initarg :info-string-getter
                       :initform (constantly ""))
   (constraint-node-test-fn
    :accessor structural-constraint/node-test-function
    :initarg :constraint-node-test-fn
    :initform nil)
   (constraint-connection-test-fn
    :accessor structural-constraint/connection-test-function
    :initarg :constraint-connection-test-fn
    :initform nil)
   (event-handler-fn-getter
    :accessor structural-constraint/event-handler-function-getter
    :initarg :event-handler-fn-getter
    :initform (constantly nil))
   (init-args-getter
    :accessor structural-constraint/init-key-arguments-getter
    :initarg :init-args-getter
    :initform (constantly nil))
   (properties-constr-fn-getter
    :accessor structural-constraint/properties-constructor-function-getter
    :initarg :properties-constr-fn-getter
    :initform (constantly nil))))

(defmethod print-object ((instance object/structural-constraint) st)
  (print-unreadable-object (instance st)
    (with-slots (name info-string-getter) instance
      (let ((info (funcall info-string-getter)))
        (format st "STRUCTURAL-CONSTRAINT ~S~A~A" name
                (if (plusp (length info)) " " "")
                info)))))

(defun make-structural-constraint (&rest args)
  (apply (alexandria:curry #'make-instance 'object/structural-constraint) args))

(defun copy-structural-constraint (constraint)
  (make-structural-constraint
   :name (structural-constraint/name constraint)
   :info-string-getter (structural-constraint/info-string-getter constraint)
   :constraint-node-test-fn
   (structural-constraint/node-test-function constraint)
   :constraint-connection-test-fn
   (structural-constraint/connection-test-function constraint)
   :event-handler-fn-getter
   (structural-constraint/event-handler-function-getter constraint)
   :init-args-getter
   (structural-constraint/init-key-arguments-getter constraint)
   :properties-constr-fn-getter
   (structural-constraint/properties-constructor-function-getter constraint)))



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

(defparameter *info-string-function-getter-containers* nil)

(defclass object/info-string-function-getter-container ()
  ((name :accessor info-string-function-getter-container/name
         :initarg :name
         :initform nil)
   (info-string-fn-getter
    :accessor info-string-function-getter-container/info-string-function-getter
    :initarg :info-string-fn-getter
    :initform nil)))

(defmethod print-object ((instance object/info-string-function-getter-container) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "INFO-STRING-GETTER-CONTAINER ~S" name))))

(defun make-info-string-function-getter-container (&rest args)
  (apply (alexandria:curry #'make-instance
                           'object/info-string-function-getter-container)
         args))

(defun copy-info-string-function-getter-container (info-container)
  (make-info-string-function-getter-container
   :name (info-string-function-getter-container/name info-container)
   :info-string-fn-getter
   (info-string-function-getter-container/info-string-function-getter info-container)))



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

(defmacro ~object-init-args-handling-let ((object-kind args) &body body)
  (alexandria:with-gensyms (info-string-fn-getter-containers
                            custom-info-string-fn-first
                            structural-constraints)
    `(let* ((,info-string-fn-getter-containers
             (getf ,args :info-string-fn-getter-containers
                   *info-string-function-getter-containers*))
            (,custom-info-string-fn-first (getf ,args :custom-info-string-fn-first))
            (,structural-constraints
             (getf ,args :structural-constraints *structural-constraints*))
            (,args (alexandria:delete-from-plist ,args
                                                 :info-string-fn-getter-containers
                                                 :custom-info-string-fn-first
                                                 :structural-constraints))
            (,args (nconc (list (gensym) nil) ,args))
            (properties-container
             (adjoin-properties
              (nconc (alexandria:ensure-list (getf ,args :properties))
                     (mapcar
                      #'(lambda (constraint)
                          (apply
                           (let ((fn (funcall
                                      (structural-constraint/properties-constructor-function-getter
                                       constraint) ,object-kind)))
                             (if fn fn (constantly nil)))
                           (iterate:iter
                            (with keys = (funcall
                                          (structural-constraint/init-key-arguments-getter
                                           constraint) ,object-kind))
                            (for tail initially ,args then (cddr tail))
                            (while (cddr tail))
                            (when (member (caddr tail) keys)
                              (nconcing (list (caddr tail) (cadddr tail)))
                              (setf (cddr tail) (cddddr tail))))))
                      ,structural-constraints))))
            (,args (cddr ,args))
            (event-handler-function
             (make-conjoint-event-handler-function
              (nconc (mapcar #'(lambda (constraint)
                                 (funcall
                                  (structural-constraint/event-handler-function-getter
                                   constraint) ,object-kind))
                             ,structural-constraints)
                     (list (getf ,args :event-handler-fn)))))
            (info-string-function
             (make-conjoint-info-string-function
              (let ((custom-fn (getf ,args :info-string-fn))
                    (fn-list
                     (mapcar
                      #'(lambda (container)
                          (funcall
                           (info-string-function-getter-container/info-string-function-getter
                            container) ,object-kind))
                      ,info-string-fn-getter-containers)))
                (if ,custom-info-string-fn-first
                    (cons custom-fn fn-list)
                    (nconc fn-list (list custom-fn)))))))
       ,@body)))
