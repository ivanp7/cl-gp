;;;; object.lisp

(in-package #:cl-gp)

;;; *** property ***

(defclass object/property ()
  ((key :reader property/key
        :initarg :key
        :initform (error "PROPERTY -- :key parameter must be supplied"))
   (value :accessor property/value
          :initarg :value
          :initform nil)
   (value-copy-fn :accessor property/value-copy-function
                  :initarg :value-copy-fn
                  :initform #'identity)))

(defun object/property? (object)
  (typep object 'object/property))

(defmethod print-object ((instance object/property) st)
  (print-unreadable-object (instance st)
    (with-slots (key value) instance
      (let ((*print-circle* nil))
        (format st "PROPERTY ~S = ~S" key value)))))

(defun make-property (key value &rest args)
  (apply (alexandria:curry #'make-instance 'object/property
                           :key key :value value) args))

(defun copy-property (property)
  (make-property (property/key property)
                 (funcall (property/value-copy-function property)
                          (property/value property))
                 :value-copy-fn (property/value-copy-function property)))

;;; *** properties ***

(defparameter *property-key-test* #'eql)

(defclass object/properties-container ()
  ((container :initarg :container
              :initform nil)))

(defun object/properties-container? (object)
  (typep object 'object/properties-container))



(defun ~properties/container (properties-cntr)
  (slot-value properties-cntr 'container))

(defun (setf ~properties/container) (new-container properties-cntr)
  (setf (slot-value properties-cntr 'container) new-container))



(defun properties/all-keys (properties-cntr)
  (if properties-cntr
      (mapcar #'property/key (~properties/container properties-cntr))))

(defmethod print-object ((instance object/properties-container) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st "PROPERTIES-CONTAINER KEYS: ~S" (properties/all-keys instance)))))

(defun properties/add-property! (properties-cntr property)
  (when properties-cntr
    (with-slots (container) properties-cntr
      (let ((old-container container))
        (pushnew property container
                 :key #'property/key
                 :test *property-key-test*)
        (not (eql container old-container))))))

(defun properties/delete-property! (properties-cntr key)
  (when properties-cntr
    (with-slots (container) properties-cntr
      (let ((old-length (length container)))
        (setf container (delete-if #'(lambda (property)
                                    (funcall *property-key-test*
                                             (property/key property) key))
                                container
                                :count 1))
        (/= (length container) old-length)))))

(defun properties/get-property (properties-cntr key)
  (when properties-cntr
    (find-if #'(lambda (property)
                 (funcall *property-key-test*
                          (property/key property) key))
             (~properties/container properties-cntr))))

(defun properties/get-list-of-properties (properties-cntr)
  (if properties-cntr
      (copy-list (~properties/container properties-cntr))))

(defun properties/get-property-value (properties-cntr key &optional default-value)
  (let ((property (properties/get-property properties-cntr key)))
    (if property
        (property/value property)
        default-value)))

(defun properties/set-property-value! (properties-cntr key new-value)
  (let ((property (properties/get-property properties-cntr key)))
    (if property
        (setf (property/value property) new-value)
        (progn
          (properties/add-property! properties-cntr (make-property key new-value))
          new-value))))



(defun make-properties-container (list-of-properties)
  (make-instance 'object/properties-container
                 :container (remove-duplicates list-of-properties
                                               :key #'property/key
                                               :test *property-key-test*)))

(defun copy-properties (properties-cntr)
  (make-properties-container (mapcar #'copy-property
                                     (~properties/container properties-cntr))))

(defun adjoin-properties (properties-list)
  (make-properties-container
   (reduce #'nconc
           (delete nil (mapcar #'(lambda (object)
                                 (if (object/properties-container? object)
                                     (properties/get-list-of-properties object)
                                     (alexandria:ensure-list object)))
                             properties-list))
           :from-end t)))

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
    (format st (object/description-string instance))))



(defun make-object (object-class &optional args)
  (apply (alexandria:curry #'make-instance object-class) args))

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
