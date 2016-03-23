;;;; type-constraint.lisp

(in-package #:cl-gp)

(defparameter *type/name-test* #'eql)

(defun type-name-equal (name1 name2)
  (funcall *type/name-test* name1 name2))



(defparameter *type/value-test* #'(lambda (source-type source-value target-value)
                                    (declare (ignore source-type))
                                    (equalp source-value target-value)))

(defclass abstract-type ()
  ((reducibility-test :reader type/reducibility-test
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
  (print-unreadable-object (instance st :identity t)
    (format st "ABSTRACT-TYPE")))

(defun type-object? (object)
  (typep object 'abstract-type))

(defgeneric type/reducible? (source-type target-type)
  (:documentation "Test if source-type can be reduced to target-type"))

(defgeneric type/subtype (type part)
  (:documentation "Extract (select) a part subtype of type"))

(defmethod type/subtype ((type abstract-type) part)
  (error "TYPE/SUBTYPE -- attempt to select subtype on a indivisible type"))

;;; *** bottom type ***

(defclass bottom-type (abstract-type)
  ())

(defmethod print-object ((instance bottom-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "BOTTOM-TYPE")))

(defun type/bottom? (type)
  (typep type 'bottom-type))

(defparameter *bottom-type* bottom-type)

;;; *** unit type ***

(defclass unit-type (abstract-type)
  ())

(defmethod print-object ((instance unit-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "UNIT-TYPE")))

(defun type/unit? (type)
  (typep type 'unit-type))

(defparameter *unit-type* unit-type)

;;; *** top type ***

(defclass top-type (abstract-type)
  ())

(defmethod print-object ((instance top-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "TOP-TYPE")))

(defun type/top? (type)
  (typep type 'top-type))

(defparameter *top-type* top-type)

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
  (print-unreadable-object (instance st :identity t)
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
    (if (or (type/bottom? instance) (type/unit? instance)
           (type/type-class? instance) (type/top? instance))
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
  (make-instance 'type/primitive :name name :reducibility-test reducibility-test
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
      (format st "RECORD ~S" fields))))

(defun make-record (fields-list &key (reducibility-test (constantly nil))
                                  (value-test *type/value-test*) properties)
  (case (length fields-list)
    (0 +type/bottom+)
    (1 (field/type (first fields-list)))
    (t (if (= (length fields-list)
              (length (remove-duplicates fields-list
                                         :key #'field/name
                                         :test *field/name-test*)))
           (make-instance 'record :fields fields-list
                          :reducibility-test reducibility-test
                          :value-test value-test :properties properties)
           (error "MAKE-RECORD -- record cannot have fields with identical names")))))

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
  (make-properties (nconc (if input-type (list :input-type input-type))
                          (if output-type (list :output-type output-type)))))

(defun make-module-type-properties (&key input-type output-type)
  (make-type-properties :input-type output-type :output-type input-type))

;;; *** type readers ***

(defun node/input-type (node)
  (get-property (node/properties node) :input-type +type/bottom+))

(defun node/output-type (node)
  (get-property (node/properties node) :output-type +type/bottom+))

(defun module/input-type (module)
  (get-property (module/world-node-properties module) :output-type +type/bottom+))

(defun module/output-type (module)
  (get-property (module/world-node-properties module) :input-type +type/bottom+))

;;; *** type constraint ***

(defun type/nested-selection (type selector)
  (if (null selector)
      type
      (type/nested-selection (type/subtype type (first selector))
                             (rest selector))))

(defparameter *type-constraint-function*
  #'(lambda (source-node target-node arrow graph)
      (declare (ignore graph))
      (let ((source-type (type/nested-selection (node/output-type source-node)
                                                (arrow/source-selector arrow)))
            (target-type (type/nested-selection (node/input-type target-node)
                                                (arrow/target-selector arrow))))
        (type/reducible? source-type target-type))))

;;; *** print functions ***

(defparameter *type-constraint/node-print-function*
  #'(lambda (plist)
      (format nil "(~S -> ~S)"
              (getf plist :input-type)
              (getf plist :output-type))))

(defparameter *type-constraint/world-node-print-function*
  #'(lambda (plist)
      (format nil "(~S <- ~S)"
              (getf plist :output-type)
              (getf plist :input-type))))

(defparameter *type-constraint/arrow-print-function*
  #'(lambda (plist)
      (format nil "(~S -> ~S)"
              (getf plist :source-type-selector)
              (getf plist :target-type-selector))))

(defparameter *type-constraint/module-print-function*
  #'(lambda (plist)
      (format nil "(~S -> ~S)"
              (getf plist :input-type)
              (getf plist :output-type))))
