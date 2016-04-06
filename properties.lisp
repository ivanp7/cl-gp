;;;; properties.lisp

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
  (mapcar #'property/key (slot-value properties 'container)))

(defmethod print-object ((instance object/properties-container) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st "PROPERTIES-CONTAINER KEYS: ~S" (properties/all-keys instance)))))

(defun properties/add-property! (properties property)
  (with-slots (container) properties
    (let ((old-container container))
      (pushnew property container
               :key #'property/key
               :test *property-key-test*)
      (not (eql container old-container)))))

(defun properties/delete-property! (properties key)
  (delete-if #'(lambda (property)
                 (funcall *property-key-test*
                          (property/key property) key))
             (slot-value properties 'container)
             :count 1))

(defun properties/get-property (properties key)
  (find-if #'(lambda (property)
               (funcall *property-key-test*
                        (property/key property) key))
           (slot-value properties 'container)))

(defun properties/get-properties-list (properties)
  (copy-list (slot-value properties 'container)))

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

;;; *** miscellaneous ***

(defun make-conjoint-info-function (fn-list)
  #'(lambda (&rest args)
      (reduce #'(lambda (str1 str2)
                  (concatenate 'string str1 str2))
              (mapcar #'(lambda (fn)
                          (apply fn args))
                      fn-list)
              :initial-value "")))
