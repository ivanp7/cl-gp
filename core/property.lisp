;;;; property.lisp

(in-package #:cl-gp)

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
   (on-value-setting-event-fn-alist :initform nil)))

(defmethod initialize-instance :after ((instance object/property) &key)
  (with-slots (value on-value-setting-event-fn-alist) instance
    (dolist (element on-value-setting-event-fn-alist)
      (funcall (cdr element) value))))

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
  (let ((new-property
         (make-property (property/key property)
                        (funcall (property/value-copy-function property)
                                 (property/value property))
                        :value-copy-fn (property/value-copy-function property)
                        :value-setting-fn (property/value-setting-function property))))
    (setf (slot-value new-property 'on-value-setting-event-fn-alist)
       (copy-alist (slot-value property
                               'on-value-setting-event-fn-alist)))
    new-property))

(defun (setf property/value) (new-value property)
  (with-slots (value value-setting-fn on-value-setting-event-fn-alist) property
    (setf value (funcall value-setting-fn value new-value))
    (dolist (entry on-value-setting-event-fn-alist)
      (funcall (cdr entry) value))
    value))



(defun property/register-value-setting-event-function! (property fn-owner fn-id func argument)
  (with-slots (on-value-setting-event-fn-alist) property
    (setf on-value-setting-event-fn-alist
       (acons (cons fn-owner fn-id) func on-value-setting-event-fn-alist))
    (funcall func argument)
    t))

(defun property/unregister-value-setting-event-function! (property fn-owner fn-id)
  (with-slots (on-value-setting-event-fn-alist) property
    (setf on-value-setting-event-fn-alist
       (delete-if #'(lambda (entry)
                      (and (tree-equal (caar entry) fn-owner)
                         (tree-equal (cdar entry) fn-id)))
                  on-value-setting-event-fn-alist))
    t))

(defun property/unregister-value-setting-event-functions! (property fn-owner)
  (with-slots (on-value-setting-event-fn-alist) property
    (setf on-value-setting-event-fn-alist
       (delete-if #'(lambda (entry)
                      (tree-equal (caar entry) fn-owner))
                  on-value-setting-event-fn-alist))
    t))

(defun property/call-value-setting-event-function (property fn-owner fn-id argument)
  (with-slots (on-value-setting-event-fn-alist) property
    (dolist (entry on-value-setting-event-fn-alist)
      (if (and (tree-equal (caar entry) fn-owner)
             (tree-equal (cdar entry) fn-id))
          (funcall (cdr entry) argument)))
    t))

(defun property/call-value-setting-event-functions (property fn-owner argument)
  (with-slots (on-value-setting-event-fn-alist) property
    (dolist (entry on-value-setting-event-fn-alist)
      (if (tree-equal (caar entry) fn-owner)
          (funcall (cdr entry) argument)))
    t))

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
