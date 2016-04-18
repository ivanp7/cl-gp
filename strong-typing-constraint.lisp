;;;; type-constraint.lisp

(in-package #:cl-gp)

;;; ****************
;;; *** entities ***
;;; ****************

(defgeneric type-entity/reducible? (source-entity target-entity)
  (:documentation "Test if source entity can be reduced to target entity"))

(defgeneric copy-type-entity (entity)
  (:documentation "Make deep copy of an entity"))

(defgeneric type-entity/has-associated-entity? (entity)
  (:documentation "Check if type entity has associated entity"))

(defgeneric type-entity/associated-entity (entity)
  (:documentation "Get associated entity of type entity"))

(defgeneric type-entity/associate-entity! (entity associated-entity setter)
  (:documentation "Set associated entity of type entity"))

(defgeneric type-entity/unassociate-entity! (entity retractor)
  (:documentation "Forget associated entity of type entity"))

(defclass abstract-type-entity ()
  ((reducibility-test-fn :accessor type-entity/reducibility-test-function
                         :initarg :reducibility-test-fn
                         :initform (constantly nil))))

(defmethod type-entity/has-associated-entity? ((entity abstract-type-entity))
  (declare (ignore entity))
  t)

(defmethod type-entity/associated-entity ((entity abstract-type-entity))
  entity)

(defmethod type-entity/associate-entity! ((entity abstract-type-entity) associated-entity setter)
  (declare (ignore entity associated-entity setter))
  nil)

(defmethod type-entity/unassociate-entity! ((entity abstract-type-entity) retractor)
  (declare (ignore entity retractor))
  nil)

;;; *** value as type ***
;;; *********************

(defclass value-as-type (abstract-type-entity)
  ((value :accessor value-as-type/value
          :initarg :value
          :initform (error "VALUE-AS-TYPE -- :value parameter must be supplied"))))

(defmethod type-entity/associated-specific-entity ((instance value-as-type))
  instance)

(defun make-value-as-type-object ()
  )

;;; *** abstract type ***
;;; *********************

(defgeneric type/subtype (type part)
  (:documentation "Extract (select) a part subtype of type"))

(defclass abstract-type-kind (abstract-type-entity)
  ())

(declaim (type t +bottom-type+))

(defmethod type/subtype ((type abstract-type) part)
  +bottom-type+)

(defun type/nested-subtype-selection (type selector)
  (if (null selector)
      type
      (type/nested-subtype-selection (type/subtype type (first selector))
                                     (rest selector))))

;;; *** special types ***

(defun abstract-special-type (abstract-type-kind)
  )

(defclass bottom-type (abstract-special-type)
  ())

(defconstant +bottom-type+ ...)

(defclass unit-type (abstract-special-type)
  ())

(defconstant +unit-type+ ...)

(defclass top-type (abstract-special-type)
  ())

(defconstant +top-type+ ...)

;;; *** primitive type ***

(defclass object/primitive-type (abstract-type-kind)
  ())

(defun make-primitive-type-object (name)
  )

;;; *** record ***

(defclass object/record (abstract-type-kind)
  ())

(defun make-record-object ()
  )

;;; *** parametric type ***

(defclass object/parametric-type (abstract-type-kind)
  ((subtype-selection-fn :accessor parametric-type/subtype-selection-function
                         :initarg :subtype-selection-fn
                         :initform (constantly +bottom-type+))))

(defun make-parametric-type-object (name parameters)
  )

;;; *** function type ***

(defclass object/function-type (abstract-type-kind)
  ())

(defun make-function-type-object ()
  )

;;; *** type class ***
;;; ******************

(defclass object/type-class (abstract-type-entity)
  (()))

(defun make-type-class-object (&optional (reducibility-test-fn (constantly t)))
  )


(defclass type-variable (abstract-type-entity)
  ((label :reader type-variable/label
          :initarg :label
          :initform (error "TYPE-VARIABLE -- :label parameter must be supplied"))
   (type-class :reader type-variable/type-class
               :initarg :type-class
               :initform (error "TYPE-VARIABLE -- :type-class parameter must be supplied"))
   (cps-connector :initform (make-cps-connector))))

(defmethod type-entity/has-associated-entity? ((entity type-variable))
  (cps-connector/has-value? (slot-value entity 'cps-connector)))

(defmethod type-entity/associated-entity ((entity type-variable))
  (cps-connector/value (slot-value entity 'cps-connector)))

(defmethod type-entity/associate-entity! ((entity type-variable) associated-entity setter)
  (cps-connector/set-value! (slot-value entity 'cps-connector) associated-entity setter))

(defmethod type-entity/unassociate-entity! ((entity type-variable) retractor)
  (cps-connector/forget-value! (slot-value entity 'cps-connector) retractor))



(defun make-type-variable (type-class-object)
  )

;;; ******************
;;; *** constraint ***
;;; ******************

;;; *** CPS constraints ***

(defclass cps-constraint/connection (cps/abstract-constraint)
  ((source-type-entity :initarg :source)
   (target-type-entity :initarg :target)))

(defmethod initialize-instance :after ((this cps-constraint/connection) &key)
  (with-slots (source-type-entity target-type-entity) this
    (cps-connector/connect! (slot-value source-type-entity 'cps-connector) this)
    (cps-connector/connect! (slot-value target-type-entity 'cps-connector) this)))

(defmethod destroy-constraint ((this cps-constraint/connection))
  ...)

(defun make-connection-cps-constraint (source-type-entity target-type-entity)
  (make-instance 'cps-constraint/connection
                 :source source-type-entity
                 :target target-type-entity))

(defmethod cps-constraint/inform-about-value ((this cps-constraint/connection))
  (with-slots (source-type-entity target-type-entity) this
    (cond ((type-entity/has-associated-entity? source-type-entity)
           (type-entity/associate-entity! target-type-entity
                                          (type-entity/associated-entity
                                           source-type-entity) this))
          ((type-entity/has-associated-entity? target-type-entity)
           (type-entity/associate-entity! source-type-entity
                                          (type-entity/associated-entity
                                           target-type-entity) this)))))

(defmethod cps-constraint/inform-about-no-value ((this cps-constraint/connection))
  (with-slots (source-type-entity target-type-entity) this
    (type-entity/unassociate-entity! source-type-entity this)
    (type-entity/unassociate-entity! target-type-entity this)
    (cps-constraint/inform-about-value this)))



(defclass cps-constraint/node (cps/abstract-constraint)
  ((type-variables-list :initarg :type-variables-list)
   (cps-constraint-fn :initarg :cps-constraint-fn
                      :initform nil)))

(defmethod initialize-instance :after ((this cps-constraint/node) &key)
  (with-slots (type-variables-list cps-constraint-fn) this
    (unless cps-constraint-fn
      (setf cps-constraint-fn
         #'(lambda (type-variables-list setter)
             (let ((set-typevar (find-if #'type-entity/has-associated-entity?
                                         type-variables-list)))
               (dolist (typevar type-variables-list)
                 (type-entity/associate-entity! typevar
                                                (type-entity/associated-entity set-typevar)
                                                setter))))))
    (dolist (typevar type-variables-list)
      (cps-connector/connect! (slot-value typevar 'cps-connector) this))))

(defmethod destroy-constraint ((this cps-constraint/node))
  ...)

(defmethod cps-constraint/inform-about-value ((this cps-constraint/node))
  (with-slots (type-variables-list) this
    (funcall (slot-value this 'cps-constraint-fn) type-variables-list this)))

(defmethod cps-constraint/inform-about-no-value ((this cps-constraint/node))
  (with-slots (type-variables-list) this
    (dolist (typevar type-variables-list)
      (type-entity/unassociate-entity! typevar this))
    (cps-constraint/inform-about-value this)))

(defun make-node-cps-constraint (type-variables-list &key cps-constraint-fn)
  (make-instance 'cps-constraint/node
                 :type-variables-list type-variables-list
                 :cps-constraint-fn cps-constraint-fn))

;;; *** generic ***

(defun object/input-type (object)
  (object/get-property-value object :input-type +bottom-type+))

(defun object/output-type (object)
  (object/get-property-value object :output-type +bottom-type+))

(defun object/type (object direction)
  (cond
    ((direction/input? direction) (object/input-type object))
    ((direction/output? direction) (object/output-type object))
    (t (error "OBJECT/TYPE -- incorrect direction ~S is supplied" direction))))

(defun object/internal-type-cps-constraints (object)
  (object/get-property-value object :internal-type-cps-constraints))

(defun make-internal-type-variables-constraint (type-variables-list &key cps-constraint-fn)
  (make-node-cps-constraint type-variables-list
                            :cps-constraint-fn cps-constraint-fn))



(defparameter *strong-typing-enabled* t)

(defparameter *strong-typing-constraint*
  (make-structural-constraint
   :name 'strong-typing
   :constraint-connection-test-fn
   #'(lambda (source-node target-node connection graph)
       (declare (ignore graph))
       (or (not *strong-typing-enabled*)
          (let ((source-entity (type/nested-subtype-selection
                                (object/output-type source-node)
                                (arrow/source-selector (connection/arrow connection))))
                (target-entity (type/nested-subtype-selection
                                (object/input-type target-node)
                                (arrow/target-selector (connection/arrow connection)))))
            (type-entity/reducible? source-entity target-entity))))
   :event-handler-fn-getter
   #'(lambda (kind)
       (alexandria:switch (kind :test #'kind-equal)
         (+kind/node+ #'(lambda (node event &rest args) ...))
         (+kind/connection+ #'(lambda (connection event &rest args) ...))
         (+kind/graph+ #'(lambda (graph event &rest args) ...))
         (t (constantly nil))))
   :init-args-getter
   #'(lambda (kind)
       (if (kind-equal kind +kind/node+)
           '(:input-type :output-type :internal-type-variables-constraints-list)))
   :properties-constr-fn-getter
   #'(lambda (kind)
       (alexandria:switch (kind :test #'kind-equal)
         (+kind/node+
          #'(lambda (&key (input-type +bottom-type+) (output-type +bottom-type+)
                  internal-type-variables-constraints-list)
              (make-properties-container
               (list (make-property :input-type input-type
                                    :value-copy-fn #'copy-type-entity)
                     (make-property :output-type output-type
                                    :value-copy-fn #'copy-type-entity)
                     (make-property :internal-type-variables-constraints-list
                                    internal-type-variables-constraints-list
                                    :value-copy-fn #'copy-cps-constraint)))))
         (+kind/connection+
          #'(lambda ()
              (make-properties-container
               (list (make-property :input-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :output-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :internal-type-variables-constraint
                                    (make-cps-constraint
                                     :value-fn #'(lambda ()
                                                   )
                                     :no-value-fn #'(lambda ()
                                                      ))
                                    :value-copy-fn #'copy-cps-constraint)))))
         (+kind/graph+
          #'(lambda ()
              (make-properties-container
               (list (make-property :input-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :output-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))))))
         (t (constantly nil))))))

(defparameter *type-info-string-function-getter-container*
  (make-info-string-function-getter-container
   :name :type
   :info-string-fn-getter
   #'(lambda (kind)
       (declare (ignore kind))
       #'(lambda (object)
           (let ((*print-circle* nil))
             (format nil "{TYPE ~S -> ~S}"
                     (object/input-type object)
                     (object/output-type object)))))))




















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defparameter *type/name-test* #'eql)

(defun type-name-equal (name1 name2)
  (funcall *type/name-test* name1 name2))



(defparameter *type/value-test* #'(lambda (source-type source-value target-value)
                                    (declare (ignore source-type))
                                    (equalp source-value target-value)))

(defclass abstract-type ()
  ((concretized-type :reader type/specialized-type
                     :initarg :concretized-type
                     :initform nil)
   (reducibility-test :reader type/reducibility-test
                      :initarg :reducibility-test
                      :initform (constantly nil))
   (value-test :reader type/value-test
               :initarg :value-test
               :initform *type/value-test*)
   (properties :accessor type/properties
               :initarg :properties
               :initform nil)))
#|
(defmethod initialize-instance :after ((instance abstract-type) &key)
  (if (alexandria:type= (type-of instance) 'abstract-type)
      (error "Abstract type object cannot be created")))
|#
(defmethod print-object ((instance abstract-type) st)
  (print-unreadable-object (instance st)
    (format st "ABSTRACT-TYPE")))

(defun type-object? (object)
  (typep object 'abstract-type))

(defgeneric type/specialized-type (type)
  (:documentation "Get concretized type for this type"))

(defgeneric type/reducible? (source-type target-type)
  (:documentation "Test if source-type can be reduced to target-type"))

(defgeneric type/subtype (type part)
  (:documentation "Extract (select) a part subtype of type"))

(defmethod type/subtype ((type abstract-type) part)
  (error "TYPE/SUBTYPE -- attempt to select subtype on a indivisible type"))

;;; *** indeterminate type ***

(defclass indeterminate-type (abstract-type)
  ((concretized-type :reader indeterminate-type/concretized-type
                     :initarg :concretized-type
                     :initform nil)))

;;; *** top type ***

(defclass top-type (indeterminate-type)
  ())

(defmethod print-object ((instance top-type) st)
  (print-unreadable-object (instance st)
    (format st "TOP-TYPE")))

(defun type/top? (type)
  (typep type 'top-type))

(defparameter *top-type* (make-instance 'top-type))

;;; *** bottom type ***

(defclass bottom-type (abstract-type)
  ())

(defmethod print-object ((instance bottom-type) st)
  (print-unreadable-object (instance st)
    (format st "BOTTOM-TYPE")))

(defun type/bottom? (type)
  (typep type 'bottom-type))

(defparameter *bottom-type* (make-instance 'bottom-type))

;;; *** unit type ***

(defclass unit-type (abstract-type)
  ())

(defmethod print-object ((instance unit-type) st)
  (print-unreadable-object (instance st)
    (format st "UNIT-TYPE")))

(defun type/unit? (type)
  (typep type 'unit-type))

(defparameter *unit-type* (make-instance 'unit-type))



;;; *** reducibility test ***

(defmethod type/reducible? ((source-type abstract-type) (target-type abstract-type))
  (cond
    ((or (type/bottom? source-type) (type/bottom? target-type)) nil)
    ((type/unit? target-type)
     (if (type/unit? source-type)
         t
         (funcall (type/reducibility-test target-type) source-type)))
    ((type/unit? source-type) nil)
    ((or (type/top? source-type) (type/top? target-type)) t)
    (t (funcall (type/reducibility-test target-type) source-type))))

;;; *** union type ***

(defclass union-type (abstract-type)
  ((name :reader union-type/name
         :initarg :name
         :initform (error "UNION-TYPE -- :name parameter must be supplied"))
   (print-info :accessor union-type/print-info
               :initarg :print-info
               :initform "")))

(defmethod print-object ((instance union-type) st)
  (print-unreadable-object (instance st)
    (with-slots (name print-info) instance
      (format st "UNION-TYPE ~S ~S" name print-info))))

(defun make-union-type (name reducibility-test &key (print-info "") properties)
  (make-instance 'union-type :name name :print-info print-info
                 :reducibility-test reducibility-test
                 :properties properties))

(defun type/union-type? (type)
  (typep type 'union-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type union-type))
  (or (and (type/union-type? source-type)
           (type-name-equal (union-type/name source-type)
                            (union-type/name target-type)))
      (call-next-method)))

;;; *** typed value ***

(defclass typed-value ()
  ((value :accessor typed-value/value
          :initarg :value
          :initform nil)
   (value-type :reader typed-value/type
               :initarg :type
               :initform *top-type*)))

(defmethod initialize-instance :after ((instance typed-value) &key)
  (with-slots (value-type) instance
    (if (or (type/bottom? instance)
            (type/unit? instance)
            (type/top? instance))
        (error "TYPED-VALUE -- value must have a specific type"))))

(defmethod print-object ((instance typed-value) st)
  (print-unreadable-object (instance st)
    (with-slots (value value-type) instance
      (format st "VALUE ~S ~S" value value-type))))

(defun make-typed-value (value value-type)
  (make-instance 'typed-value :value value :type value-type))

(defun typed-value-object? (object)
  (typep object 'typed-value))

(defun typed-value/reducible? (source-typed-value target-typed-value)
  (if (type/reducible? (typed-value/type source-typed-value)
                       (typed-value/type target-typed-value))
      (funcall (type/value-test (typed-value/type target-typed-value))
               (typed-value/type source-typed-value)
               (typed-value/value source-typed-value)
               (typed-value/value target-typed-value))))

;;; *** parametric type ***

(defclass parametric-type (abstract-type)
  ((name :reader parametric-type/name
         :initarg :name
         :initform (error "PARAMETRIC-TYPE -- :name parameter must be supplied"))
   (arguments :reader parametric-type/arguments
              :initarg :arguments
              :initform (error "PARAMETRIC-TYPE -- :arguments parameter must be supplied"))))

(defmethod print-object ((instance parametric-type) st)
  (print-unreadable-object (instance st)
    (with-slots (name arguments) instance
      (format st "PARAMETRIC-TYPE ~S ~S" name arguments))))

(declaim (ftype function make-primitive-type))

(defun make-parametric-type (name arguments-list &key (reducibility-test (constantly nil))
                                                   (value-test *type/value-test*)
                                                   properties)
  (if (zerop (length arguments-list))
      (make-primitive-type name)
      (if (every #'(lambda (arg)
                     (or (type-object? arg)
                         (typed-value-object? arg)))
                 arguments-list)
          (make-instance 'parametric-type :name name :arguments arguments-list
                         :reducibility-test reducibility-test
                         :value-test value-test :properties properties))))

(defun type/parametric? (type)
  (typep type 'parametric-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type parametric-type))
  (or (and (type/parametric? source-type)
           (type-name-equal (parametric-type/name source-type)
                            (parametric-type/name target-type))
           (= (length (parametric-type/arguments source-type))
              (length (parametric-type/arguments target-type)))
           (every #'(lambda (source-arg target-arg)
                      (cond
                        ((and (type-object? source-arg)
                              (type-object? target-arg))
                         (type/reducible? source-arg target-arg))
                        ((and (typed-value-object? source-arg)
                              (typed-value-object? target-arg))
                         (typed-value/reducible? source-arg target-arg))))
                  (parametric-type/arguments source-type)
                  (parametric-type/arguments target-type)))
      (call-next-method)))

;;; *** primitive type ***

(defclass primitive-type (parametric-type)
  ((name :reader primitive-type/name
         :initarg :name
         :initform (error "PRIMITIVE-TYPE -- :name parameter must be supplied"))
   (arguments :initform nil)))

(defmethod print-object ((instance primitive-type) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "PRIMITIVE-TYPE ~S" name))))

(defun make-primitive-type (name &key (reducibility-test (constantly nil))
                                   (value-test *type/value-test*)
                                   properties)
  (make-instance 'primitive-type :name name :reducibility-test reducibility-test
                 :value-test value-test :properties properties))

(defun type/primitive? (type)
  (typep type 'primitive-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type primitive-type))
  (or (and (type/primitive? source-type)
           (type-name-equal (primitive-type/name source-type)
                            (primitive-type/name target-type)))
      (call-next-method)))

;;; *** record ***

(defparameter *field/name-test* #'eql)

(defclass field ()
  ((name :reader field/name
         :initarg :name
         :initform (error "FIELD -- :name parameter must be supplied"))
   (type :reader field/type
         :initarg :type
         :initform (error "FIELD -- :type parameter must be supplied"))))

(defmethod print-object ((instance field) st)
  (print-unreadable-object (instance st)
    (with-slots (name type) instance
      (format st "FIELD ~S : ~S" name type))))

(defun make-field (name type)
  (make-instance 'field :name name :type type))



(defclass record (abstract-type)
  ((fields :reader record/fields
           :initarg :fields
           :initform (error "RECORD -- :fields parameter must be supplied"))))

(defmethod print-object ((instance record) st)
  (print-unreadable-object (instance st)
    (with-slots (fields) instance
      (format st "RECORD FIELDS: ~S" fields))))

(defun make-record (fields-list &key (reducibility-test (constantly nil))
                                  (value-test *type/value-test*) properties)
  (if (zerop (length fields-list))
      *bottom-type*
      (if (= (length fields-list)
             (length (remove-duplicates fields-list
                                        :key #'field/name
                                        :test *field/name-test*)))
          (make-instance 'record :fields fields-list
                         :reducibility-test reducibility-test
                         :value-test value-test :properties properties)
          (error "MAKE-RECORD -- record cannot have fields with identical names"))))

(defun type/record? (type)
  (typep type 'record))

(defmethod type/subtype ((type record) part)
  (with-slots (fields) type
    (let ((field (find part fields
                       :key #'field/name :test *field/name-test*)))
      (if field
          (field/type field)
          (error "TYPE/SUBTYPE -- record doesn't have field with a name ~S" part)))))

(defmethod type/reducible? ((source-type abstract-type) (target-type record))
  (or (and (type/record? source-type)
           (and (= (length (record/fields source-type))
                   (length (record/fields target-type)))
                (every #'(lambda (target-field)
                           (find-if #'(lambda (source-field)
                                        (and (funcall *field/name-test*
                                                      (field/name source-field)
                                                      (field/name target-field))
                                             (type/reducible? (field/type source-field)
                                                              (field/type target-field))))
                                    (record/fields source-type)))
                       (record/fields target-type))))
      (call-next-method)))

;;; *** function type ***

(defclass function-type (abstract-type)
  ((argument :reader function-type/argument
             :initarg :argument
             :initform (error "FUNCTION-TYPE -- :argument parameter must be supplied"))
   (result :reader function-type/result
           :initarg :result
           :initform (error "FUNCTION-TYPE -- :result parameter must be supplied"))))

(defmethod print-object ((instance function-type) st)
  (print-unreadable-object (instance st)
    (with-slots (argument result) instance
      (format st "FUNCTION-TYPE (~S -> ~S)" argument result))))

(defun make-function-type (argument result &key (reducibility-test (constantly nil))
                                             (value-test *type/value-test*)
                                             properties)
  (make-instance 'function-type :argument argument :result result
                 :reducibility-test reducibility-test
                 :value-test value-test :properties properties))

(defun type/function? (type)
  (typep type 'function-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type function-type))
  (or (and (type/function? source-type)
           (type/reducible? (function-type/argument source-type)
                            (function-type/argument target-type))
           (type/reducible? (function-type/result source-type)
                            (function-type/result target-type)))
      (call-next-method)))

;;; *** type properties ***

(defun make-type-properties (&key input-type output-type)
  (apply #'make-properties (nconc (if input-type (list :original-input-type input-type))
                                  (if output-type (list :original-output-type output-type)))))

(defun make-module-type-properties (&key input-type output-type)
  (make-type-properties :input-type output-type :output-type input-type))

;;; *** type getters ***

(macrolet ((define-type-getter (fn-name key default opp-key opp-default error-msg)
             `(defun ,fn-name (object)
                (cond
                  ((object/node? object)
                   (if (node/primitive? object)
                       (get-property (node/properties object) ,key ,default)
                       (get-property (node/active-properties object)
                                     ,opp-key ,opp-default)))
                  ((object/module? object)
                   (get-property (module/world-node-properties object)
                                 ,opp-key ,opp-default))
                  (t (error ,error-msg))))))
  (define-type-getter object/original-input-type
      :original-input-type *bottom-type*
      :original-output-type *bottom-type*
      "OBJECT/ORIGINAL-INPUT-TYPE -- unknown object type")
  (define-type-getter object/original-output-type
      :original-output-type *bottom-type*
      :original-input-type *bottom-type*
      "OBJECT/ORIGINAL-OUTPUT-TYPE -- unknown object type")
  (define-type-getter object/actual-input-type
      :actual-input-type (object/original-input-type object)
      :actual-output-type (object/original-output-type object)
      "OBJECT/ACTUAL-INPUT-TYPE -- unknown object type")
  (define-type-getter object/actual-output-type
      :actual-output-type (object/original-output-type object)
      :actual-input-type (object/original-input-type object)
      "OBJECT/ACTUAL-OUTPUT-TYPE -- unknown object type"))

;;; *** type constraint ***

(defun type/nested-selection (type selector)
  (if (null selector)
      type
      (type/nested-selection (type/subtype type (first selector))
                             (rest selector))))

(defparameter *type-constraint-function*
  #'(lambda (source-node target-node connection graph)
      (declare (ignore graph))
      (let ((source-type (type/nested-selection (object/actual-output-type source-node)
                                                (arrow/source-selector arrow)))
            (target-type (type/nested-selection (object/actual-input-type target-node)
                                                (arrow/target-selector arrow))))
        (type/reducible? source-type target-type))))

;;; *** print functions ***

(defparameter *type-constraint/info-function*
  #'(lambda (object properties)
      (declare (ignore properties))
      (format nil "(~S -> ~S)"
              (object/actual-input-type object)
              (object/actual-output-type object))))

(defparameter *type-constraint/world-node-info-function*
  #'(lambda (object properties)
      (declare (ignore properties))
      (format nil "(~S <- ~S)"
              (object/actual-output-type object)
              (object/actual-input-type object))))
|#
