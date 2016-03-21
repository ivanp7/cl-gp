;;;; type.lisp

(in-package #:cl-gp)

(defparameter *type/name-test* #'eql)

(defclass abstract-type ()
  ((name :reader type/name
         :initarg :name
         :initform nil)))
#|
(defmethod initialize-instance :after ((instance abstract-type) &key)
  (if (alexandria:type= (type-of instance) 'abstract-type)
      (error "Abstract type object cannot be created")))
|#
(defmethod print-object ((instance abstract-type) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (name) instance
      (format st "ABSTRACT-TYPE ~S" name))))

(defun type-object? (object)
  (typep object 'abstract-type))

(defgeneric type/reducible? (source-type target-type)
  (:documentation "Test if source-type can be reduced to target-type"))

(defmethod type/reducible? ((source-type abstract-type) (target-type abstract-type))
  (funcall *type/name-test* (type/name source-type) (type/name target-type)))

(defgeneric type/subtype (type part)
  (:documentation "Extract (select) a part subtype of type"))

(defmethod type/subtype ((type abstract-type) part)
  (error "TYPE/SUBTYPE -- attempt to select subtype on a indivisible type"))



(defmacro define-type-constant (constant-name kind &rest args)
  `(alexandria:define-constant ,constant-name (make-instance ',kind ,@args)
     :test #'(lambda (val1 val2)
               (alexandria:type= (type-of val1)
                                 (type-of val2)))))

;;; *** top type ***

(defclass top-type (abstract-type)
  ())

(defmethod print-object ((instance top-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "TOP-TYPE")))

(defun type/top? (type)
  (typep type 'top-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type top-type))
  t)

(define-type-constant +top-type+ top-type)

;;; *** void type ***

(defclass void-type (abstract-type)
  ())

(defmethod print-object ((instance void-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "VOID-TYPE")))

(defun type/void? (type)
  (typep type 'void-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type void-type))
  (type/void? source-type))

(define-type-constant +void-type+ void-type)

;;; *** bottom type ***

(defclass bottom-type (abstract-type)
  ())

(defmethod print-object ((instance bottom-type) st)
  (print-unreadable-object (instance st :identity t)
    (format st "BOTTOM-TYPE")))

(defun type/bottom? (type)
  (typep type 'bottom-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type bottom-type))
  nil)

(define-type-constant +bottom-type+ bottom-type)

;;; *** typed value ***

(defclass typed-value ()
  ((value :accessor typed-value/value
          :initarg :value
          :initform nil)
   (value-type :reader typed-value/type
               :initarg :type
               :initform +top-type+)))

(defmethod print-object ((instance typed-value) st)
  (print-unreadable-object (instance st)
    (with-slots (value value-type) instance
      (format st "VALUE ~S ~S" value value-type))))

(defun make-typed-value (value value-type)
  (make-instance 'typed-value :value value :type value-type))

(defun typed-value-object? (object)
  (typep object 'typed-value))



(defgeneric type/value-reducible? (source-value source-type target-value target-type)
  (:documentation "Test if source-value of source-type can be reduced to target-value of target-type"))

(defmethod type/value-reducible? (source-value (source-type abstract-type)
                                  target-value (target-type abstract-type))
  (declare (ignore source-type target-type))
  (equalp source-value target-value))

(defun typed-value/reducible? (source-typed-value target-typed-value)
  (if (type/reducible? (typed-value/type source-typed-value)
                       (typed-value/type target-typed-value))
      (type/value-reducible? (typed-value/value source-typed-value)
                             (typed-value/type source-typed-value)
                             (typed-value/value target-typed-value)
                             (typed-value/type target-typed-value))))

;;; *** parametric type ***

(defclass parametric-type (abstract-type)
  ((arguments :reader parametric-type/arguments
              :initarg :arguments
              :initform (error "PARAMETRIC-TYPE -- :arguments parameter must be supplied"))))

(defmethod print-object ((instance parametric-type) st)
  (print-unreadable-object (instance st)
    (with-slots (name arguments) instance
      (format st "PARAMETRIC-TYPE ~S ~S" name arguments))))

(declaim (ftype function make-primitive-type))

(defun make-parametric-type (name arguments-list)
  (if (zerop (length arguments-list))
      (make-primitive-type name)
      (if (every #'(lambda (arg)
                 (or (type-object? arg)
                    (typed-value-object? arg)))
             arguments-list)
          (make-instance 'parametric-type :name name :arguments arguments-list))))

(defun type/parametric? (type)
  (typep type 'parametric-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type parametric-type))
  (and (type/parametric? source-type)
     (funcall *type/name-test* (type/name source-type) (type/name target-type))
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
        (parametric-type/arguments target-type))))

;;; *** primitive type ***

(defclass primitive-type (parametric-type)
  ((arguments :initform nil)))

(defmethod print-object ((instance primitive-type) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "PRIMITIVE-TYPE ~S" name))))

(defun make-primitive-type (name)
  (make-instance 'type/primitive :name name))

(defun type/primitive? (type)
  (typep type 'primitive-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type primitive-type))
  (and (type/primitive? source-type)
     (funcall *type/name-test* (type/name source-type) (type/name target-type))))

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

(defun make-record (fields-list)
  (case (length fields-list)
    (0 +type/bottom+)
    (1 (field/type (first fields-list)))
    (t (if (= (length fields-list)
              (length (remove-duplicates fields-list
                                         :key #'field/name
                                         :test *field/name-test*)))
           (make-instance 'record :fields fields-list)
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
  (and (type/record? source-type)
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
           (record/fields target-type)))))

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

(defun make-function-type (argument result)
  (make-instance 'function-type :argument argument :result result))

(defun type/function? (type)
  (typep type 'function-type))

(defmethod type/reducible? ((source-type abstract-type) (target-type function-type))
  (and (type/function? source-type)
     (type/reducible? (function-type/argument source-type)
                      (function-type/argument target-type))
     (type/reducible? (function-type/result source-type)
                      (function-type/result target-type))))

;;; *** node type data ***

(defun node/type (node)
  (getf (node/properties node) :type))

(defun node/input-type (node)
  (if (node/call? node)
      (function-type/argument (node/type node))
      +type/bottom+))

(defun node/output-type (node)
  (if (node/call? node)
      (function-type/result (node/type node))
      (node/type node)))

;;; *** arrow type selector ***

(defun arrow/source-type-selector (arrow)
  (getf (arrow/properties arrow) :source-type-selector))

(defun arrow/target-type-selector (arrow)
  (getf (arrow/properties arrow) :target-type-selector))

;;; *** type constraint ***

(defun type/nested-selection (type selector)
  (if (null selector)
      type
      (type/nested-selection (type/subtype type (first selector))
                             (rest selector))))

(defparameter *type-constraint-function*
  #'(lambda (source-node target-node arrow graph)
      (declare (ignore graph))
      (type/reducible? (type/nested-selection (node/type source-node)
                                              (arrow/source-type-selector arrow))
                       (type/nested-selection (node/type target-node)
                                              (arrow/target-type-selector arrow)))))

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
