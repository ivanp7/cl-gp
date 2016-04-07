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

(defun properties/all-keys (properties)
  (if properties
      (mapcar #'property/key (slot-value properties 'container))))

(defmethod print-object ((instance object/properties-container) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st "PROPERTIES-CONTAINER KEYS: ~S" (properties/all-keys instance)))))

(defun properties/add-property! (properties property)
  (when properties
    (with-slots (container) properties
      (let ((old-container container))
        (pushnew property container
                 :key #'property/key
                 :test *property-key-test*)
        (not (eql container old-container))))))

(defun properties/delete-property! (properties key)
  (when properties
    (with-slots (container) properties
      (let ((old-length (length container)))
        (setf container (delete-if #'(lambda (property)
                                    (funcall *property-key-test*
                                             (property/key property) key))
                                container
                                :count 1))
        (/= (length container) old-length)))))

(defun properties/get-property (properties key)
  (when properties
    (find-if #'(lambda (property)
                 (funcall *property-key-test*
                          (property/key property) key))
             (slot-value properties 'container))))

(defun properties/get-properties-list (properties)
  (if properties
      (copy-list (slot-value properties 'container))))

(defun properties/get-property-value (properties key &optional default-value)
  (let ((property (properties/get-property properties key)))
    (if property
        (property/value property)
        default-value)))

(defun properties/set-property-value! (properties key new-value)
  (let ((property (properties/get-property properties key)))
    (if property
        (setf (property/value property) new-value)
        (error "PROPERTIES/SET-PROPERTY-VALUE! -- property ~S doesn't exist" key))))



(defun make-properties-container (list-of-properties)
  (make-instance 'object/properties-container
                 :container (remove-duplicates list-of-properties
                                               :key #'property/key
                                               :test *property-key-test*)))

(defun copy-properties (properties)
  (make-properties-container (mapcar #'copy-property
                                     (slot-value properties 'container))))

;;; *** abstract object ***

(defparameter *purpose-test* #'eql)

(defun purpose-equal (purpose1 purpose2)
  (funcall *purpose-test* purpose1 purpose2))

(defconstant +purpose/regular+ :regular)

(defclass abstract-object ()
  ((purpose :reader object/purpose
            :initarg :purpose
            :initform +purpose/regular+)
   (properties :accessor object/properties
               :initarg :properties
               :initform nil)
   (events-handler-fn :accessor object/events-handler-function
                      :initarg :events-handler-fn
                      :initform (constantly nil))
   (info-string-fn :accessor object/info-string-function
                   :initarg :info-string-function
                   :initform (constantly ""))))

(defgeneric object/description-string (object &key no-object-class-name)
  (:documentation "Generate description string for printing purposes"))

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
                (if (null (getf args :events-handler-fn))
                    (list :events-handler-fn (object/events-handler-function object)))
                (if (null (getf args :info-string-fn))
                    (list :info-string-fn (object/info-string-function object)))
                args)))

;;; *** miscellaneous ***

(defun make-conjoint-events-handler (events-handlers-list)
  #'(lambda (&rest args)
      (dolist (fn events-handlers-list)
        (apply fn args))
      nil))

(defun make-conjoint-info-function (info-functions-list)
  (if (null info-functions-list)
      (constantly "")
      #'(lambda (&rest args)
          (reduce #'(lambda (str1 str2)
                      (concatenate 'string str1 " " str2))
                  (mapcar #'(lambda (fn)
                              (apply fn args))
                          info-functions-list)))))
