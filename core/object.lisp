;;;; object.lisp

(in-package #:cl-gp)

;;; *** abstract object ***

(defparameter *purpose-test* #'eql)

(defun purpose-equal (purpose1 purpose2)
  (funcall *purpose-test* purpose1 purpose2))

(defconstant +purpose/regular+ 'regular)



(defparameter *kind-test* #'eql)

(defun kind-equal (kind1 kind2)
  (funcall *kind-test* kind1 kind2))

(defgeneric object/kind (object)
  (:documentation "Get object class identifier value"))



(defclass abstract-object ()
  ((purpose :reader object/purpose
            :initarg :purpose
            :initform +purpose/regular+)
   (properties :accessor object/properties
               :initarg :properties
               :initform nil)
   (event-handler-fn :accessor object/event-handler-function
                     :initarg :event-handler-fn
                     :initform (constantly nil))
   (info-string-fn :accessor object/info-string-function
                   :initarg :info-string-fn
                   :initform (constantly ""))))

(defconstant +kind/abstract+ 'object)

(defmethod object/kind ((object abstract-object))
  +kind/abstract+)

(defgeneric object/description-string (object &key no-object-kind)
  (:documentation "Generate description string for printing purposes"))

(defmacro define-description-string-method (object-class &body body)
  `(defmethod object/description-string ((object ,object-class) &key no-object-kind)
     (let ((descr (progn ,@body)))
       (if no-object-kind
           descr
           (format nil "~S~A~A" (object/kind object)
                   (if (plusp (length descr)) " " "")
                   descr)))))

(defmethod print-object ((instance abstract-object) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st (object/description-string instance)))))



(defun make-object (object-class &optional args)
  (let ((object (apply (alexandria:curry #'make-instance object-class) args)))
    (funcall (object/event-handler-function object) object :on-initialization)
    object))

(defun copy-object (object &optional args)
  (apply (alexandria:curry #'make-instance (type-of object))
         (nconc (if (null (getf args :purpose))
                    (list :purpose (object/purpose object)))
                (if (null (getf args :properties))
                    (list :properties (copy-properties (object/properties object))))
                (if (null (getf args :event-handler-fn))
                    (list :event-handler-fn (object/event-handler-function object)))
                (if (null (getf args :info-string-fn))
                    (list :info-string-fn (object/info-string-function object)))
                args)))

(defun object/get-property-value (object key &optional default-value)
  (properties/get-property-value (object/properties object) key default-value))

(defun object/set-property-value! (object key new-value)
  (if (object/properties object)
      (properties/set-property-value! (object/properties object) key new-value)
      (progn
        (setf (object/properties object)
           (make-properties-container
            (list (make-property key new-value))))
        new-value)))
