;;;; property.lisp

(in-package #:cl-gp)

;;; *** function collection ***

(defparameter *function-collection-owner-test* #'eql)
(defparameter *function-collection-id-test* #'eql)

(defclass object/function-collection ()
  ((collection :initarg :collection
               :initform nil)
   (application-rule-fn :accessor function-collection/application-rule-function
                        :initarg :application-rule-fn
                        :initform #'(lambda (fn-list &optional args)
                                      (dolist (fn fn-list)
                                        (apply fn args))
                                      nil))))

(defun object/function-collection? (object)
  (typep object 'object/function-collection))

(defmethod initialize-instance :after ((instance object/function-collection) &key)
  (with-slots (collection) instance
    (setf collection (copy-list collection))))

(defmethod print-object ((instance object/function-collection) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st "FUNCTION-COLLECTION"))))

(defun ~fn-collection/collection (fn-collection)
  (slot-value fn-collection 'collection))

(defun (setf ~fn-collection/collection) (new-collection fn-collection)
  (setf (slot-value fn-collection 'collection) new-collection))

(defun ~make-fn-collection-entry (owner id func)
  (cons (cons owner id) func))

(defun ~fn-collection-entry/owner (entry)
  (caar entry))

(defun ~fn-collection-entry/id (entry)
  (cdar entry))

(defun ~fn-collection-entry/function (entry)
  (cdr entry))



(defun function-collection/add-function! (fn-collection fn-owner fn-id func
                                          &optional do-1st-call args)
  (with-slots (collection application-rule-fn) fn-collection
    (push (~make-fn-collection-entry fn-owner fn-id func) collection)
    (if do-1st-call
        (funcall application-rule-fn (list func) args))))

(defun function-collection/delete-function! (fn-collection fn-owner fn-id)
  (with-slots (collection) fn-collection
    (setf collection
       (delete-if #'(lambda (entry)
                      (and (funcall *function-collection-owner-test*
                                  (~fn-collection-entry/owner entry) fn-owner)
                         (funcall *function-collection-id-test*
                                  (~fn-collection-entry/id entry) fn-id)))
                  collection))
    nil))

(defun function-collection/delete-functions! (fn-collection fn-owner)
  (with-slots (collection) fn-collection
    (setf collection
       (delete-if #'(lambda (entry)
                      (funcall *function-collection-owner-test*
                               (~fn-collection-entry/owner entry) fn-owner))
                  collection))
    nil))

(defun function-collection/call-function (fn-collection fn-owner fn-id &optional args)
  (with-slots (collection application-rule-fn) fn-collection
    (funcall application-rule-fn
             (mapcar #'~fn-collection-entry/function
                     (remove-if-not
                      #'(lambda (entry)
                          (and (funcall *function-collection-owner-test*
                                      (~fn-collection-entry/owner entry) fn-owner)
                             (funcall *function-collection-id-test*
                                      (~fn-collection-entry/id entry) fn-id)))
                      collection))
             args)))

(defun function-collection/call-functions (fn-collection fn-owner &optional args)
  (with-slots (collection application-rule-fn) fn-collection
    (funcall application-rule-fn
             (mapcar #'~fn-collection-entry/function
                     (remove-if-not
                      #'(lambda (entry)
                          (funcall *function-collection-owner-test*
                                   (~fn-collection-entry/owner entry) fn-owner))
                      collection))
             args)))

(defun function-collection/call-all-functions (fn-collection &optional args)
  (with-slots (collection application-rule-fn) fn-collection
    (funcall application-rule-fn
             (mapcar #'~fn-collection-entry/function collection)
             args)))



(defun make-function-collection (&optional application-rule-fn)
  (if application-rule-fn
      (make-instance 'object/function-collection :application-rule-fn application-rule-fn)
      (make-instance 'object/function-collection)))

(defun copy-function-collection (fn-collection)
  (make-instance 'object/function-collection
                 :collection (copy-tree (~fn-collection/collection fn-collection))
                 :application-rule-fn
                 (function-collection/application-rule-function fn-collection)))

;;; *** property ***

(alexandria:define-constant +property/writable+
    #'(lambda (old-value new-value)
        (declare (ignore old-value))
        new-value)
  :test (constantly t))

(alexandria:define-constant +property/read-only+
    #'(lambda (old-value new-value)
        (declare (ignore new-value))
        old-value)
  :test (constantly t))

(defclass object/property ()
  ((key :reader property/key
        :initarg :key
        :initform (error "PROPERTY -- :key parameter must be supplied"))
   (value :reader property/value
          :initarg :value
          :initform nil)
   (value-copy-fn :accessor property/value-copy-function
                  :initarg :value-copy-fn
                  :initform #'identity)
   (value-setting-fn :accessor property/value-setting-function
                     :initarg :value-setting-fn
                     :initform +property/writable+)
   (value-setting-event-handler-collection
    :accessor property/value-setting-event-handler-collection
    :initarg :value-setting-event-handler-collection
    :initform (make-function-collection))))

(defmethod initialize-instance :after ((instance object/property) &key)
  (with-slots (value value-setting-event-handler-collection) instance
    (function-collection/call-all-functions
     value-setting-event-handler-collection (list value))))

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
                 :value-copy-fn (property/value-copy-function property)
                 :value-setting-fn (property/value-setting-function property)
                 :value-setting-event-handler-collection
                 (copy-function-collection
                  (property/value-setting-event-handler-collection property))))

(defun (setf property/value) (new-value property)
  (with-slots (value value-setting-fn value-setting-event-handler-collection) property
    (setf value (funcall value-setting-fn value new-value))
    (function-collection/call-all-functions
     value-setting-event-handler-collection (list value))
    value))

(defun property/force-value! (property new-value)
  (let ((value-setting-fn (property/value-setting-function property)))
    (setf (property/value-setting-function property) +property/writable+)
    (setf (property/value property) new-value)
    (setf (property/value-setting-function property) value-setting-fn)
    new-value))

;;; *** properties ***

(defparameter *property-key-test* #'eql)

(defclass object/property-collection ()
  ((collection :initarg :collection
               :initform nil)))

(defun object/property-collection? (object)
  (typep object 'object/property-collection))

(defmethod initialize-instance :after ((instance object/property-collection) &key)
  (with-slots (collection) instance
    (setf collection (copy-list collection))))

(defun ~prop-collection/collection (prop-collection)
  (slot-value prop-collection 'collection))

(defun (setf ~prop-collection/collection) (new-collection prop-collection)
  (setf (slot-value prop-collection 'collection) new-collection))



(defun properties/all-keys (prop-collection)
  (if prop-collection
      (mapcar #'property/key (~prop-collection/collection prop-collection))))

(defmethod print-object ((instance object/property-collection) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st "PROPERTY-COLLECTION: ~:S" (properties/all-keys instance)))))

(defun properties/add-property! (prop-collection property)
  (when prop-collection
    (with-slots (collection) prop-collection
      (let ((old-container collection))
        (pushnew property collection
                 :key #'property/key
                 :test *property-key-test*)
        (not (eql collection old-container))))))

(defun properties/delete-property! (prop-collection key)
  (when prop-collection
    (with-slots (collection) prop-collection
      (let ((old-length (length collection)))
        (setf collection (delete-if #'(lambda (property)
                                     (funcall *property-key-test*
                                              (property/key property) key))
                                 collection
                                 :count 1))
        (/= (length collection) old-length)))))

(defun properties/get-property (prop-collection key)
  (when prop-collection
    (find-if #'(lambda (property)
                 (funcall *property-key-test*
                          (property/key property) key))
             (~prop-collection/collection prop-collection))))

(defun properties/get-list-of-properties (prop-collection)
  (if prop-collection
      (copy-list (~prop-collection/collection prop-collection))))

(defun properties/get-property-value (prop-collection key &optional default-value)
  (let ((property (properties/get-property prop-collection key)))
    (if property
        (property/value property)
        default-value)))

(defun properties/set-property-value! (prop-collection key new-value)
  (let ((property (properties/get-property prop-collection key)))
    (if property
        (setf (property/value property) new-value)
        (progn
          (properties/add-property! prop-collection (make-property key new-value))
          new-value))))

(defun properties/force-property-value! (prop-collection key new-value)
  (let ((property (properties/get-property prop-collection key)))
    (if (null property)
        (properties/set-property-value! prop-collection key new-value)
        (property/force-value! property new-value))))



(defun make-property-collection (list-of-properties)
  (if (= (length list-of-properties)
         (length (remove-duplicates list-of-properties
                                    :key #'property/key
                                    :test *property-key-test*)))
      (make-instance 'object/property-collection
                     :collection list-of-properties)
      (error "MAKE-PROPERTY-COLLECTION -- collection cannot contain properties with identical keys")))

(defun copy-properties (prop-collection)
  (make-property-collection
   (mapcar #'copy-property
           (~prop-collection/collection prop-collection))))

(defun adjoin-properties (properties-list)
  (make-property-collection
   (reduce #'nconc
           (delete nil (mapcar #'(lambda (object)
                                 (if (object/property-collection? object)
                                     (properties/get-list-of-properties object)
                                     (alexandria:ensure-list object)))
                             properties-list))
           :from-end t)))
