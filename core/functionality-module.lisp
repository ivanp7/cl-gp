;;;; functionality-module.lisp

(in-package #:cl-gp)

(defparameter *functionality-modules* nil)

(defparameter *functionality-module-name-test* #'eql)

(defclass functionality-module ()
  ((name :accessor functionality-module/name
         :initarg :name
         :initform (error "FUNCTIONALITY-MODULE -- :name parameter must be supplied"))
   (dependencies-register-fn :accessor functionality-module/dependencies-register-function
                             :initarg :dependencies-register-fn
                             :initform (constantly nil))
   (info-string-fn :accessor functionality-module/info-string-function
                   :initarg :info-string-fn
                   :initform (constantly ""))
   (constraint-fn-getter
    :accessor functionality-module/constraint-function-getter
    :initarg :constraint-fn-getter
    :initform (constantly nil))
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
        (if (plusp (length info))
            (format st "FUNCTIONALITY-MODULE ~S ~A" name info)
            (format st "FUNCTIONALITY-MODULE ~S" name))))))

(defun make-functionality-module (&rest args)
  (apply (alexandria:curry #'make-instance 'functionality-module) args))

(defun copy-functionality-module (module)
  (make-functionality-module
   :name (functionality-module/name module)
   :dependencies-register-fn
   (functionality-module/dependencies-register-function module)
   :info-string-fn (functionality-module/info-string-function module)
   :constraint-fn-getter
   (functionality-module/constraint-function-getter module)
   :event-handler-fn-getter
   (functionality-module/event-handler-function-getter module)
   :init-args-getter
   (functionality-module/init-key-arguments-getter module)
   :properties-constr-fn-getter
   (functionality-module/properties-constructor-function-getter module)))



(defun register-functionality-module! (module)
  (let ((old-registry *functionality-modules*)
        (new-registry (pushnew module *functionality-modules*
                               :test *functionality-module-name-test*
                               :key #'functionality-module/name)))
    (unless (eql new-registry old-registry)
      (funcall (functionality-module/dependencies-register-function module))
      t)))

;;; *** functionality info string function package ***

(defparameter *info-string-function-packages* nil)

(defparameter *info-string-function-package-name-test* #'eql)

(defclass object/info-string-function-package ()
  ((name :accessor info-string-function-package/name
         :initarg :name
         :initform (error "INFO-STRING-FUNCTION-PACKAGE -- :name parameter must be supplied"))
   (info-string-fn-getter
    :accessor info-string-function-package/info-string-function-getter
    :initarg :info-string-fn-getter
    :initform (constantly nil))))

(defmethod print-object ((instance object/info-string-function-package) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "INFO-STRING-FUNCTION-PACKAGE ~S" name))))

(defun make-functionality-info-string-function-package (&rest args)
  (apply (alexandria:curry #'make-instance
                           'object/info-string-function-package)
         args))

(defun copy-functionality-info-string-function-package (info-package)
  (make-functionality-info-string-function-package
   :name (info-string-function-package/name info-package)
   :info-string-fn-getter
   (info-string-function-package/info-string-function-getter info-package)))



(defun register-info-string-function-package! (pckg)
  (not (eql *info-string-function-packages*
          (pushnew pckg *info-string-function-packages*
                   :test *info-string-function-package-name-test*
                   :key #'info-string-function-package/name))))
