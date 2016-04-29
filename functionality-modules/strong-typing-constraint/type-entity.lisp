;;;; type-entity.lisp

(in-package #:cl-gp)

;;; ****************
;;; *** entities ***
;;; ****************

(defgeneric type-entity/description-string (entity &key no-class-name)
  (:documentation "Generate description string for printing purposes"))

(defgeneric copy-type-entity (entity &rest args)
  (:documentation "Make deep copy of an entity"))

(defgeneric type-entity/has-associated-entity? (entity)
  (:documentation "Check if type entity has associated entity"))

(defgeneric type-entity/associated-entity (entity)
  (:documentation "Get associated entity of type entity"))

(defgeneric type-entity/associate-entity! (entity associated-entity setter)
  (:documentation "Set associated entity of type entity"))

(defgeneric type-entity/unassociate-entity! (entity retractor)
  (:documentation "Forget associated entity of type entity"))

(defgeneric type-entity/reducibility-test (source-entity target-entity)
  (:documentation "Test if source entity can be reduced to target entity"))

(defgeneric type-entity/component-type (entity component)
  (:documentation "Extract (select) type of a component of data"))



(defclass abstract-type-entity ()
  ())

(defmethod print-object ((instance abstract-type-entity) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st (type-entity/description-string instance)))))

(defun copy-abstract-type-entity (entity &optional args)
  (apply (alexandria:curry #'make-instance (type-of entity)) args))

(defmethod copy-type-entity ((entity abstract-type-entity) &rest args)
  (copy-abstract-type-entity entity args))

(defmethod type-entity/has-associated-entity? ((entity abstract-type-entity))
  (declare (ignore entity))
  nil)

(defmethod type-entity/associated-entity ((entity abstract-type-entity))
  (declare (ignore entity))
  nil)

(defmethod type-entity/associate-entity! ((entity abstract-type-entity) associated-entity setter)
  (declare (ignore entity associated-entity setter))
  nil)

(defmethod type-entity/unassociate-entity! ((entity abstract-type-entity) retractor)
  (declare (ignore entity retractor))
  nil)

(defun type-entity/actual-entity (entity)
  (if (type-entity/has-associated-entity? entity)
      (type-entity/actual-entity (type-entity/associated-entity entity))
      entity))



(defun type-entity/reducible? (source-entity target-entity)
  (let ((source-entity (type-entity/actual-entity source-entity))
        (target-entity (type-entity/actual-entity target-entity)))
    (type-entity/reducibility-test source-entity target-entity)))

(defmethod type-entity/reducibility-test ((source-entity abstract-type-entity)
                                          (target-entity abstract-type-entity))
  nil)

(defun reducibility-test-result-min (result1 result2)
  (cond ((null result1) result1)
        ((null result2) result2)
        ((eql result1 t) result2)
        ((eql result2 t) result1)
        (t :loss)))

(defun reducibility-test-result-max (result1 result2)
  (cond ((null result1) result2)
        ((null result2) result1)
        ((eql result1 t) result1)
        ((eql result2 t) result2)
        (t :loss)))

(defun type/recursive-component-type-selection (entity selector)
  (if (null selector)
      (type-entity/actual-entity entity)
      (type/recursive-component-type-selection
       (type-entity/component-type
        (type-entity/actual-entity entity)
        (first selector))
       (rest selector))))

;;; *** abstract type ***
;;; *********************

(defclass abstract-type (abstract-type-entity)
  ())

;;; *** special types ***
;;; *********************

(defclass abstract-special-type (abstract-type)
  ())

(defclass object/bottom-type (abstract-special-type)
  ())

(defmethod type-entity/description-string ((entity object/bottom-type) &key no-class-name)
  (declare (ignore entity))
  (if (not no-class-name)
      "BOTTOM-TYPE" ""))

(defun type/bottom-type? (entity)
  (typep entity 'object/bottom-type))

(defun bottom-type ()
  (load-time-value (make-instance 'object/bottom-type) t))

(defclass object/top-type (abstract-special-type)
  ())

(defmethod type-entity/description-string ((entity object/top-type) &key no-class-name)
  (declare (ignore entity))
  (if (not no-class-name)
      "TOP-TYPE" ""))

(defun type/top-type? (entity)
  (typep entity 'object/top-type))

(defun top-type ()
  (load-time-value (make-instance 'object/top-type) t))



(defmethod type-entity/component-type ((entity abstract-type-entity) component)
  (bottom-type))

;;; *** data types ***
;;; ******************

(defclass abstract-data-type (abstract-type)
  ((reducibility-test-fn :accessor type/reducibility-test-function
                         :initarg :reducibility-test-fn
                         :initform (constantly nil))
   (properties :accessor type/properties
               :initarg :properties
               :initform nil)))

(defun make-data-type (type-class &optional args)
  (apply (alexandria:curry #'make-instance type-class) args))

(defun copy-abstract-data-type (entity &optional args)
  (copy-abstract-type-entity
   entity (nconc (if (null (getf args :reducibility-test-fn))
                     (list :reducibility-test-fn
                           (type/reducibility-test-function entity)))
                 (if (null (getf args :properties))
                     (list :properties
                           (type/properties entity)))
                 args)))

(defmethod copy-type-entity ((entity abstract-data-type) &rest args)
  (copy-abstract-data-type entity args))

(defmethod type-entity/reducibility-test ((source-entity abstract-special-type)
                                          (target-entity abstract-special-type))
  (and (type/top-type? source-entity)
     (type/top-type? target-entity)))

(defmethod type-entity/reducibility-test ((source-entity abstract-special-type)
                                          (target-entity abstract-data-type))
  (declare (ignore target-entity))
  (if (type/top-type? source-entity) :loss))

(defmethod type-entity/reducibility-test ((source-entity abstract-data-type)
                                          (target-entity abstract-special-type))
  (declare (ignore source-entity))
  (type/top-type? target-entity))

(defmethod type-entity/reducibility-test ((source-entity abstract-data-type)
                                          (target-entity abstract-data-type))
  (let ((result (funcall (type/reducibility-test-function target-entity)
                         source-entity target-entity :target)))
    (if (eql result t)
        t
        (reducibility-test-result-max
         result (funcall (type/reducibility-test-function source-entity)
                         source-entity target-entity :source)))))

(defmacro define-type-reducibility-test-method (source-type-class target-type-class &body body)
  `(defmethod type-entity/reducibility-test ((source-entity ,source-type-class)
                                             (target-entity ,target-type-class))
     (let ((result (progn ,@body)))
       (if (eql result t)
           t
           (reducibility-test-result-max
            result (call-next-method))))))



(defun type/get-property-value (entity key &optional default-value)
  (properties/get-property-value (type/properties entity) key default-value))

(defun type/set-property-value! (entity key new-value)
  (if (type/properties entity)
      (properties/set-property-value! (type/properties entity) key new-value)
      (progn
        (setf (type/properties entity)
           (make-property-collection
            (list (make-property key new-value))))
        new-value)))

;;; *** abstract solid data type ***

(defclass abstract-solid-data-type (abstract-data-type)
  ())

;;; *** record (product type) ***

(defparameter *field-name-test* #'eql)

(defun field-name-equal (name1 name2)
  (funcall *field-name-test* name1 name2))



(defclass object/field ()
  ((name :reader field/name
         :initarg :name
         :initform (error "FIELD -- :name parameter must be supplied"))
   (type-entity :reader field/type
                :initarg :type
                :initform (bottom-type))))

(defun field/description-string (field)
  (let ((*print-circle* nil))
    (format nil "{~A :: ~A}"
            (field/name field)
            (type-entity/description-string (field/type field)))))

(defmethod print-object ((instance object/field) st)
  (print-unreadable-object (instance st)
    (format st "~A" (field/description-string instance))))

(defun make-field (name type-entity)
  (make-instance 'object/field :name name :type type-entity))

(defun copy-field (field)
  (make-field (field/name field) (copy-type-entity (field/type field))))



(defclass object/record (abstract-data-type)
  ((fields :accessor record/fields
           :initarg :fields
           :initform nil)))

(defun type/record? (entity)
  (typep entity 'object/record))

(defun type/unit-type? (entity)
  (and (type/record? entity)
     (null (record/fields entity))))

(defmethod initialize-instance :after ((instance object/record) &key)
  (with-slots (fields) instance
    (setf fields (copy-list fields))))

(defmethod type-entity/description-string ((entity object/record) &key no-class-name)
  (let ((*print-circle* nil))
    (with-slots (fields) entity
      (if (null fields)
          (if (not no-class-name)
              "UNIT-TYPE" "")
          (format nil (if (not no-class-name)
                        "{RECORD: ~:A}" "~:A")
                  (mapcar #'field/description-string (record/fields entity)))))))

(defun make-record (fields-list &rest args)
  (if (= (length fields-list)
         (length (remove-duplicates fields-list
                                    :test *field-name-test*
                                    :key #'field/name)))
      (make-data-type 'object/record
                      (nconc (list :fields fields-list)
                             (alexandria:remove-from-plist args :fields)))
      (error "MAKE-RECORD -- record cannot have fields with identical names")))

(defun unit-type ()
  (load-time-value (make-record nil) t))

(defun copy-record (record &optional args)
  (copy-abstract-data-type
   record (nconc (if (null (getf args :fields))
                     (list :fields (mapcar #'copy-field (record/fields record))))
                 args)))

(defmethod copy-type-entity ((entity object/record) &rest args)
  (copy-record entity args))

(define-type-reducibility-test-method abstract-solid-data-type object/record
  (let ((fields (remove-if #'type/bottom-type? (record/fields target-entity)
                           :key #'field/type)))
    (case (length fields)
      (0 :loss)
      (1 (type-entity/reducible? source-entity (field/type (first fields))))
      (t nil))))

(define-type-reducibility-test-method object/record abstract-solid-data-type
  (let ((fields (remove-if #'type/bottom-type? (record/fields source-entity)
                           :key #'field/type)))
    (case (length fields)
      (0 nil)
      (1 (type-entity/reducible? (field/type (first fields)) target-entity))
      (t (let ((c (count-if #'(lambda (entity)
                                (not (null (type-entity/reducible? entity target-entity))))
                            fields :key #'field/type)))
           (if (= c 1) :loss))))))

(define-type-reducibility-test-method object/record object/record
  (let* ((src-fields (copy-list (record/fields source-entity)))
         (src-fields-bottom-type (remove-if-not #'type/bottom-type? src-fields
                                                :key #'field/type))
         (tgt-fields (copy-list (record/fields target-entity)))
         (tgt-fields-bottom-type (remove-if-not #'type/bottom-type? tgt-fields
                                                :key #'field/type)))
    (unless (or (intersection src-fields-bottom-type tgt-fields
                             :test *field-name-test* :key #'field/name)
               (intersection tgt-fields-bottom-type src-fields
                             :test *field-name-test* :key #'field/name))
      (let* ((src-fields (nset-difference src-fields src-fields-bottom-type
                                          :test *field-name-test* :key #'field/name))
             (tgt-fields (nset-difference tgt-fields tgt-fields-bottom-type
                                          :test *field-name-test* :key #'field/name))
             (matching-fields-names (mapcar #'field/name
                                            (intersection src-fields tgt-fields
                                                          :test *field-name-test*
                                                          :key #'field/name)))
             (src-matching-fields (remove-if-not
                                   #'(lambda (name) (member name matching-fields-names
                                                  :test *field-name-test*))
                                   src-fields :key #'field/name))
             (tgt-matching-fields-shuffled (remove-if-not
                                            #'(lambda (name) (member name matching-fields-names
                                                           :test *field-name-test*))
                                            tgt-fields :key #'field/name))
             (src-extra-fields (set-difference src-fields src-matching-fields
                                               :test *field-name-test* :key #'field/name))
             (tgt-extra-fields (set-difference tgt-fields tgt-matching-fields-shuffled
                                               :test *field-name-test* :key #'field/name)))
        (if (not (null tgt-extra-fields))
            nil
            (let* ((tgt-matching-fields
                    (mapcar #'(lambda (src-field)
                                (find-if #'(lambda (tgt-field-name)
                                             (funcall *field-name-test*
                                                      (field/name src-field)
                                                      tgt-field-name))
                                         tgt-matching-fields-shuffled
                                         :key #'field/name))
                            src-matching-fields))
                   (result (reduce #'reducibility-test-result-min
                                   (mapcar #'(lambda (src-field tgt-field)
                                               (type-entity/reducible? (field/type src-field)
                                                                       (field/type tgt-field)))
                                           src-matching-fields tgt-matching-fields)
                                   :initial-value t)))
              (reducibility-test-result-min result
                                            (if (not (null src-extra-fields))
                                                :loss t))))))))

(defmethod type-entity/component-type ((entity object/record) component)
  (let ((field (find component (record/fields entity)
                     :test *field-name-test* :key #'field/name)))
    (if (null field)
        (bottom-type)
        (field/type field))))

;;; *** function type ***

(defclass object/function-type (abstract-solid-data-type)
  ((argument :accessor function-type/argument
             :initarg :argument
             :initform (bottom-type))
   (result :accessor function-type/result
           :initarg :result
           :initform (bottom-type))))

(defun type/function-type? (entity)
  (typep entity 'object/function-type))

(defmethod type-entity/description-string ((entity object/function-type) &key no-class-name)
  (let ((*print-circle* nil))
    (format nil (if (not no-class-name)
                  "{FUNCTION-TYPE: ~A -> ~A}" "{~A -> ~A}")
            (type-entity/description-string (function-type/argument entity))
            (type-entity/description-string (function-type/result entity)))))

(defun make-function-type (argument-type result-type &rest args)
  (make-data-type 'object/function-type
                  (nconc (list :argument argument-type
                               :result result-type)
                         (alexandria:remove-from-plist args :argument :result))))

(defun copy-function-type (entity &optional args)
  (copy-abstract-data-type
   entity (nconc (if (null (getf args :argument))
                     (list :argument
                           (copy-type-entity (function-type/argument entity))))
                 (if (null (getf args :result))
                     (list :result
                           (copy-type-entity (function-type/result entity))))
                 args)))

(defmethod copy-type-entity ((entity object/function-type) &rest args)
  (copy-function-type entity args))

(define-type-reducibility-test-method object/function-type object/function-type
  (reducibility-test-result-min
   (type-entity/reducible? (function-type/argument source-entity)
                           (function-type/argument target-entity))
   (type-entity/reducible? (function-type/result source-entity)
                           (function-type/result target-entity))))

;;; *** parametric type ***

(defparameter *parametric-type-name-test* #'eql)

(defun type-name-equal (name1 name2)
  (funcall *parametric-type-name-test* name1 name2))

(defclass object/parametric-type (abstract-solid-data-type)
  ((name :accessor parametric-type/name
         :initarg :name
         :initform (error "PARAMETRIC-TYPE -- :name parameter must be supplied"))
   (parameter :accessor parametric-type/parameter
              :initarg :parameter
              :initform (unit-type))
   (component-type-fn :accessor parametric-type/component-type-function
                      :initarg :component-type-fn
                      :initform (constantly (bottom-type)))))

(defun type/parametric-type? (entity)
  (typep entity 'object/parametric-type))

(defun type/primitive-type? (entity)
  (and (type/parametric-type? entity)
     (type/unit-type? (parametric-type/parameter entity))))

(defmethod type-entity/description-string ((entity object/parametric-type)
                                           &key no-class-name)
  (let ((*print-circle* nil))
    (with-slots (name parameter) entity
      (if (type/unit-type? parameter)
          (format nil (if (not no-class-name)
                        "{PRIMITIVE-TYPE ~S}" "~S") name)
          (format nil (if (not no-class-name)
                        "{PARAMETRIC-TYPE ~S: ~A}" "{~S: ~A}") name
                        (type-entity/description-string parameter
                                                        :no-class-name t))))))

(defun make-parametric-type (name parameters-record &rest args)
  (make-data-type 'object/parametric-type
                  (nconc (list :name name :parameter parameters-record)
                         (alexandria:remove-from-plist args :name :parameter))))

(defun make-primitive-type (name &rest args)
  (apply (alexandria:curry #'make-parametric-type name (unit-type)) args))

(defun primitive-type/name (object)
  (parametric-type/name object))

(defun copy-parametric-type (entity &optional args)
  (copy-abstract-data-type
   entity (nconc (if (null (getf args :name))
                     (list :name (parametric-type/name entity)))
                 (if (null (getf args :parameter))
                     (list :parameter
                           (copy-type-entity (parametric-type/parameter entity))))
                 (if (null (getf args :component-type-fn))
                     (list :component-type-fn
                           (parametric-type/component-type-function entity)))
                 args)))

(defmethod copy-type-entity ((entity object/parametric-type) &rest args)
  (copy-parametric-type entity args))

(define-type-reducibility-test-method object/parametric-type object/parametric-type
  (and (funcall *parametric-type-name-test*
              (parametric-type/name source-entity)
              (parametric-type/name target-entity))
     (type-entity/reducible? (parametric-type/parameter source-entity)
                             (parametric-type/parameter target-entity))))

(defmethod type-entity/component-type ((entity object/parametric-type) component)
  (funcall (parametric-type/component-type-function entity) entity component))

;;; *** type class ***
;;; ******************

(defparameter *type-class-name-test* #'eql)

(defun type-class-name-equal (name1 name2)
  (funcall *type-class-name-test* name1 name2))

(defclass object/type-class ()
  ((name :reader type-class/name
         :initarg :name
         :initform (error "TYPE-CLASS -- :name parameter must be supplied"))
   (properties :accessor type-class/properties
               :initarg :properties
               :initform nil)
   (reducibility-test-fn :accessor type-class/reducibility-test-function
                         :initarg :reducibility-test-fn
                         :initform (constantly nil))
   (class-reducibility-test-fn :accessor type-class/class-reducibility-test-function
                               :initarg :class-reducibility-test-fn
                               :initform (constantly nil))))

(defun type-class/description-string (type-class)
  (with-slots (name) type-class
    (let ((*print-circle* nil))
      (format nil "TYPE-CLASS ~S" name))))

(defmethod print-object ((instance object/type-class) st)
  (print-unreadable-object (instance st)
    (format st "~A" (type-class/description-string instance))))

(defun make-type-class (name reducibility-test-fn class-reducibility-test-fn &key properties)
  (make-instance 'object/type-class
                 :name name
                 :properties properties
                 :reducibility-test-fn reducibility-test-fn
                 :class-reducibility-test-fn class-reducibility-test-fn))

(defun copy-type-class (type-class)
  (make-type-class (type-class/name type-class)
                   (type-class/reducibility-test-function type-class)
                   (type-class/class-reducibility-test-function type-class)
                   :properties (copy-properties (type-class/properties type-class))))

(defun type-class/top-class? (type-class)
  (type-class-name-equal (type-class/name type-class) 'top))

(defun top-type-class ()
  (load-time-value (make-type-class 'top
                                    #'(lambda (source target fn-owner)
                                        (declare (ignore source target))
                                        (cond
                                          ((eql fn-owner :target) t)
                                          ((eql fn-owner :source) :loss)))
                                    #'(lambda (source target fn-owner)
                                        (declare (ignore source))
                                        (cond
                                          ((eql fn-owner :target) t)
                                          ((eql fn-owner :source)
                                           (if (type-class/top-class? target)
                                               t :loss)))))
                   t))

;;; *** type variable ***

(defclass object/type-variable (abstract-type-entity)
  ((name :reader type-variable/name
         :initarg :name
         :initform (error "TYPE-VARIABLE -- :name parameter must be supplied"))
   (type-class :reader type-variable/type-class
               :initarg :type-class
               :initform (error "TYPE-VARIABLE -- :type-class parameter must be supplied"))
   (cps-connector :initform (make-cps-connector))))

(defun type/type-variable? (entity)
  (typep entity 'object/type-variable))

(defmethod type-entity/description-string ((entity object/type-variable)
                                           &key no-class-name)
  (let ((*print-circle* nil))
    (let ((descr (with-slots (name type-class) entity
                   (format nil "~S: ~A" name
                           (type-class/description-string type-class)))))
      (if (not no-class-name)
          (concatenate 'string "{TYPE-VARIABLE " descr "}")
          (concatenate 'string "{" descr "}")))))

(defun make-type-variable (name &optional (type-class-object (top-type-class)) &rest args)
  (apply (alexandria:curry #'make-instance 'object/type-variable
                           :name name
                           :type-class type-class-object)
         (alexandria:remove-from-plist args :label :type-class)))

(defun copy-type-variable (entity &optional args)
  (copy-abstract-type-entity
   (nconc (if (null (getf args :name))
              (list :name (type-variable/name entity)))
          (if (null (getf args :type-class))
              (list :type-class (copy-type-class (type-variable/type-class entity))))
          args)))

(defmethod copy-type-entity ((entity object/type-variable) &rest args)
  (copy-type-variable entity args))

(defmethod type-entity/has-associated-entity? ((entity object/type-variable))
  (cps-connector/has-value? (slot-value entity 'cps-connector)))

(defmethod type-entity/associated-entity ((entity object/type-variable))
  (with-slots (cps-connector) entity
    (if (cps-connector/has-value? cps-connector)
        (cps-connector/value cps-connector))))

(defmethod type-entity/associate-entity! ((entity object/type-variable) associated-entity setter)
  (cps-connector/set-value! (slot-value entity 'cps-connector) associated-entity setter))

(defmethod type-entity/unassociate-entity! ((entity object/type-variable) retractor)
  (cps-connector/forget-value! (slot-value entity 'cps-connector) retractor))



(defmethod type-entity/reducibility-test ((source-entity abstract-special-type)
                                          (target-entity object/type-variable))
  (if (type/top-type? source-entity)
      (if (type-class/top-class? (type-variable/type-class target-entity))
          t :loss)))

(defmethod type-entity/reducibility-test ((source-entity object/type-variable)
                                          (target-entity abstract-special-type))
  (declare (ignore source-entity))
  (type/top-type? target-entity))

(defmethod type-entity/reducibility-test ((source-entity abstract-data-type)
                                          (target-entity object/type-variable))
  (funcall (type-class/reducibility-test-function
            (type-variable/type-class target-entity))
           source-entity
           (type-variable/type-class target-entity) :target))

(defmethod type-entity/reducibility-test ((source-entity object/type-variable)
                                          (target-entity abstract-data-type))
  (funcall (type-class/reducibility-test-function
            (type-variable/type-class source-entity))
           (type-variable/type-class source-entity)
           target-entity :source))

(defmethod type-entity/reducibility-test ((source-entity object/type-variable)
                                          (target-entity object/type-variable))
  (let ((result (funcall (type-class/class-reducibility-test-function
                          (type-variable/type-class target-entity))
                         (type-variable/type-class source-entity)
                         (type-variable/type-class target-entity) :target)))
    (if (eql result t)
        t
        (reducibility-test-result-max
         result
         (funcall (type-class/class-reducibility-test-function
                   (type-variable/type-class source-entity))
                  (type-variable/type-class source-entity)
                  (type-variable/type-class target-entity) :source)))))
