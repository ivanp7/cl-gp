;;;; structural-constraints.lisp

(in-package #:cl-gp)

(defparameter *functionality-modules* nil)

(defclass functionality-module ()
  ((name :accessor functionality-module/name
         :initarg :name
         :initform nil)
   (info-string-fn :accessor functionality-module/info-string-function
                   :initarg :info-string-fn
                   :initform (constantly ""))
   (constraint-node-test-fn
    :accessor functionality-module/node-test-function
    :initarg :constraint-node-test-fn
    :initform nil)
   (constraint-connection-test-fn
    :accessor functionality-module/connection-test-function
    :initarg :constraint-connection-test-fn
    :initform nil)
   (event-handler-fn-getter
    :accessor functionality-module/event-handler-function-getter
    :initarg :event-handler-fn-getter
    :initform (constantly nil))
   (init-args-getter
    :accessor functionality-module/init-key-arguments-getter
    :initarg :init-args-getter
    :initform (constantly nil))
   (properties-constr-fn-getter
    :accessor functionality-module/properties-constructor-function-getter
    :initarg :properties-constr-fn-getter
    :initform (constantly nil))))

(defmethod print-object ((instance functionality-module) st)
  (print-unreadable-object (instance st)
    (with-slots (name info-string-fn) instance
      (let ((info (funcall info-string-fn)))
        (format st "FUNCTIONALITY-MODULE ~S~A~A" name
                (if (plusp (length info)) " " "")
                info)))))

(defun make-functionality-module (&rest args)
  (apply (alexandria:curry #'make-instance 'functionality-module) args))

(defun copy-functionality-module (module)
  (make-functionality-module
   :name (functionality-module/name module)
   :info-string-fn (functionality-module/info-string-function module)
   :constraint-node-test-fn
   (functionality-module/node-test-function module)
   :constraint-connection-test-fn
   (functionality-module/connection-test-function module)
   :event-handler-fn-getter
   (functionality-module/event-handler-function-getter module)
   :init-args-getter
   (functionality-module/init-key-arguments-getter module)
   :properties-constr-fn-getter
   (functionality-module/properties-constructor-function-getter module)))



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

;;; *** functionality info string function package ***

(defparameter *functionality-info-string-function-packages* nil)

(defclass object/functionality-info-string-function-package ()
  ((name :accessor functionality-info-string-function-package/name
         :initarg :name
         :initform nil)
   (info-string-fn-getter
    :accessor functionality-info-string-function-package/info-string-function-getter
    :initarg :info-string-fn-getter
    :initform nil)))

(defmethod print-object ((instance object/functionality-info-string-function-package) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "INFO-STRING-GETTER-CONTAINER ~S" name))))

(defun make-functionality-info-string-function-package (&rest args)
  (apply (alexandria:curry #'make-instance
                           'object/functionality-info-string-function-package)
         args))

(defun copy-functionality-info-string-function-package (info-package)
  (make-functionality-info-string-function-package
   :name (functionality-info-string-function-package/name info-package)
   :info-string-fn-getter
   (functionality-info-string-function-package/info-string-function-getter info-package)))



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
  (alexandria:with-gensyms (funct-info-string-fn-packages
                            custom-info-string-fn-first
                            functionality-modules)
    `(let* ((,funct-info-string-fn-packages
             (getf ,args :info-string-fn-getter-containers
                   *functionality-info-string-function-packages*))
            (,custom-info-string-fn-first (getf ,args :custom-info-string-fn-first))
            (,functionality-modules
             (getf ,args :structural-constraints *functionality-modules*))
            (,args (alexandria:delete-from-plist ,args
                                                 :info-string-fn-getter-containers
                                                 :custom-info-string-fn-first
                                                 :structural-constraints))
            (,args (nconc (list (gensym) nil) ,args))
            (properties-container
             (adjoin-properties
              (nconc (alexandria:ensure-list (getf ,args :properties))
                     (mapcar
                      #'(lambda (module)
                          (apply
                           (let ((fn (funcall
                                      (functionality-module/properties-constructor-function-getter
                                       module) ,object-kind)))
                             (if fn fn (constantly nil)))
                           (iterate:iter
                             (with keys = (funcall
                                           (functionality-module/init-key-arguments-getter
                                            module) ,object-kind))
                             (for tail initially ,args then (cddr tail))
                             (while (cddr tail))
                             (when (member (caddr tail) keys)
                               (nconcing (list (caddr tail) (cadddr tail)))
                               (setf (cddr tail) (cddddr tail))))))
                      ,functionality-modules))))
            (,args (cddr ,args))
            (event-handler-function
             (make-conjoint-event-handler-function
              (nconc (mapcar #'(lambda (module)
                                 (funcall
                                  (functionality-module/event-handler-function-getter
                                   module) ,object-kind))
                             ,functionality-modules)
                     (list (getf ,args :event-handler-fn)))))
            (info-string-function
             (make-conjoint-info-string-function
              (let ((custom-fn (getf ,args :info-string-fn))
                    (fn-list
                     (mapcar
                      #'(lambda (container)
                          (funcall
                           (functionality-info-string-function-package/info-string-function-getter
                            container) ,object-kind))
                      ,funct-info-string-fn-packages)))
                (if ,custom-info-string-fn-first
                    (cons custom-fn fn-list)
                    (nconc fn-list (list custom-fn)))))))
       ,@body)))
