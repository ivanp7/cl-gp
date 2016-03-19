;;;; type.lisp

(in-package #:cl-gp)

(defclass type/abstract ()
  ((test= :reader type/value-test=
          :initarg :test=
          :initform (constantly nil))))

(defmethod initialize-instance :after ((instance type/abstract) &key)
  (with-slots (test=) instance
    (unless (functionp test=)
      (error "TYPE -- test= must be a function")))
  (if (alexandria:type= (type-of instance) 'type/abstract)
      (error "TYPE/ABSTRACT -- abstract type cannot be made")))

(defmethod print-object ((instance type/abstract) st)
  (print-unreadable-object (instance st)
    (format st "TYPE/ABSTRACT")))



(defun type-object? (object)
  (typep object 'type/abstract))

(defun type/kind (object)
  (type-of object))

(defun type-of-kind? (type kind)
  (typep type kind))

(defmacro kind= (k1 k2)
  `(alexandria:type= ,k1 ,k2))

(defmacro define-type-constant (constant-name kind &rest args)
  `(alexandria:define-constant ,constant-name (make-instance ',kind ,@args)
     :test #'(lambda (val1 val2)
               (kind= (type/kind val1)
                      (type/kind val2)))))



(defparameter *type/name-test* #'eql)

;;; *** type class ***

(defclass type/class (type/abstract)
  ((name :reader type-class/name
         :initarg :name
         :initform (error "TYPE/CLASS -- :name parameter must be supplied"))
   (type-test :reader type-class/type-test
              :initarg :type-test
              :initform (error "TYPE/CLASS -- :type-test parameter must be supplied"))))

(defmethod print-object ((instance type/class) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "TYPE-CLASS: ~S" name))))

(defun make-type-class (name type-test)
  (make-instance 'type/class :name name :type-test type-test))

(defun type/class? (type)
  (typep type 'type/class))

;;; *** top type ***

(defclass type/top (type/class)
  ((name :initform 'top-type-class)
   (type-test :initform (constantly t))))

(defun type/top? (type)
  (typep type 'type/top))

(define-type-constant +type/top+ type/top)

;;; *** void type ***

(declaim (ftype function type/void?))

(defclass type/void (type/class)
  ((name :initform 'void-type-class)
   (type-test :initform #'type/void?)))

(defun type/void? (type)
  (typep type 'type/void))

(define-type-constant +type/void+ type/void)

;;; *** bottom type ***

(defclass type/bottom (type/class)
  ((name :initform 'bottom-type-class)
   (type-test :initform (constantly nil))))

(defun type/bottom? (type)
  (typep type 'type/bottom))

(define-type-constant +type/bottom+ type/bottom)

;;; *** parametric type ***

(defclass type/parametric (type/abstract)
  ((name :reader parametric-type/name
         :initarg :name
         :initform (error "TYPE/PARAMETRIC -- :name parameter must be supplied"))
   (arguments :initarg :arguments
              :initform (error "TYPE/PARAMETRIC -- :arguments parameter must be supplied"))))

(defmethod initialize-instance :after ((instance type/parametric) &key)
  (with-slots (arguments) instance
    (setf arguments (copy-list arguments))))

(defmethod print-object ((instance type/parametric) st)
  (print-unreadable-object (instance st)
    (with-slots (name arguments) instance
      (format st "PARAMETRIC-TYPE: ~S of ~S" name arguments))))

(declaim (ftype function make-primitive-type))

(defun make-parametric-type (name arguments-list &optional (test= #'equalp))
  (if (zerop (length arguments-list))
      (make-primitive-type name test=)
      (make-instance 'type/parametric :name name :arguments arguments-list :test= test=)))

(defun parametric-type/arguments (parametric)
  (with-slots (arguments) parametric
    (copy-list arguments)))

(defun type/parametric? (type)
  (typep type 'type/parametric))

;;; *** primitive type ***

(defclass type/primitive (type/parametric)
  ((name :reader primitive-type/name
         :initform (error "TYPE/PRIMITIVE -- :name parameter must be supplied"))
   (arguments :initform nil)))

(defmethod print-object ((instance type/primitive) st)
  (print-unreadable-object (instance st)
    (with-slots (name) instance
      (format st "PRIMITIVE-TYPE: ~S" name))))

(defun make-primitive-type (name &optional (test= #'equalp))
  (make-instance 'type/primitive :name name :test= test=))

(defun type/primitive? (type)
  (typep type 'type/primitive))

;;; *** field&record ***

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
      (format st "FIELD ~S as ~S" name type))))

(defun make-field (name type)
  (make-instance 'field :name name :type type))



(defclass type/record (type/abstract)
  ((fields :initarg :fields
           :initform (error "TYPE/RECORD -- :fields parameter must be supplied"))))

(defmethod initialize-instance :after ((instance type/record) &key)
  (with-slots (fields) instance
    (setf fields (copy-list fields))))

(defmethod print-object ((instance type/record) st)
  (print-unreadable-object (instance st)
    (with-slots (fields) instance
      (format st "RECORD: ~S" fields))))

(defun make-record (fields-list &optional (test= #'equalp))
  (case (length fields-list)
    (0 +type/bottom+)
    (1 (field/type (first fields-list)))
    (t (if (= (length fields-list)
              (length (remove-duplicates fields-list
                                         :key #'field/name
                                         :test *field/name-test*)))
           (make-instance 'type/record :fields fields-list :test= test=)
           (error "MAKE-RECORD -- record cannot have fields with identical names")))))

(defun record/fields (record)
  (with-slots (fields) record
    (copy-list fields)))

(defun type/record? (type)
  (typep type 'type/record))

(defun record/nested-search (type fields-names-list)
  (if (null fields-names-list)
      type
      (if (type/record? type)
          (with-slots (fields) type
            (let ((field (find (car fields-names-list) fields
                               :key #'field/name :test *field/name-test*)))
              (if field
                  (record/nested-search
                   (field/type field) (cdr fields-names-list))
                  +type/bottom+)))
          +type/bottom+)))

;;; *** function type ***

(defclass type/function (type/abstract)
  ((argument :reader function-type/argument
             :initarg :argument
             :initform (error "TYPE/FUNCTION -- :argument parameter must be supplied"))
   (result :reader function-type/result
           :initarg :result
           :initform (error "TYPE/FUNCTION -- :result parameter must be supplied"))))

(defmethod print-object ((instance type/function) st)
  (print-unreadable-object (instance st)
    (with-slots (argument result) instance
      (format st "FUNCTION-TYPE: ~S -> ~S" argument result))))

(defun make-function-type (argument result &optional (test= #'equalp))
  (make-instance 'type/function :argument argument :result result :test= test=))

(defun type/function? (type)
  (typep type 'type/function))

;;; *** typed value ***

(defclass typed-value ()
  ((object :accessor typed-value/value
           :initarg :value
           :initform nil)
   (object-type :reader typed-value/type
                :initarg :type
                :initform +type/bottom+)))

(defmethod initialize-instance :after ((instance typed-value) &key)
  (with-slots (object-type) instance
    (cond
      ((not (type-object? object-type))
       (error "TYPED-VALUE -- invalid object type is specified"))
      ((type/class? object-type)
       (error "TYPED-VALUE -- cannot create object of a type class")))))

(defmethod print-object ((instance typed-value) st)
  (print-unreadable-object (instance st)
    (with-slots (object object-type) instance
      (format st "VALUE: ~S as ~S" object object-type))))

(defun make-typed-value (object object-type)
  (make-instance 'typed-value :value object :type object-type))

(defun typed-value-object? (object)
  (typep object 'typed-value))

(defun typed-value/equal? (val1 val2)
  (if (eql (type/value-test= (typed-value/type val1))
           (type/value-test= (typed-value/type val2)))
      (funcall (type/value-test= (typed-value/type val1))
               (typed-value/value val1) (typed-value/value val2))
      (or (funcall (type/value-test= (typed-value/type val1))
                  (typed-value/value val1) (typed-value/value val2))
         (funcall (type/value-test= (typed-value/type val2))
                  (typed-value/value val1) (typed-value/value val2)))))

;;; *** type compatibility test ***

(defun type/compatible? (source-type target-type)





  !!!FIX!!!







  (cond
    ((and (typed-value-object? obj1) (typed-value-object? obj2))
     (and (type/compatible? (typed-value/type obj1)
                          (typed-value/type obj2))
        (typed-value/equal? obj1 obj2)))
    ((not (and (type-object? obj1) (type-object? obj2))) nil)
    ((and (type-of-kind? obj1 'type/class)
        (type-of-kind? obj2 'type/class))
     (and (not (type/bottom? obj1)) (not (type/bottom? obj2))
        (funcall *type/name-test* (type-class/name obj1) (type-class/name obj2))
        (eql (type-class/type-test obj1) (type-class/type-test obj2))))
    ((or (type-of-kind? obj1 'type/class)
        (type-of-kind? obj2 'type/class))
     (if (type-of-kind? obj1 'type/class)
         (funcall (type-class/type-test obj1) obj2)
         (funcall (type-class/type-test obj2) obj1)))
    ((and (type-of-kind? obj1 'type/parametric)
        (type-of-kind? obj2 'type/parametric))
     (and (funcall *type/name-test*
                 (parametric-type/name obj1)
                 (parametric-type/name obj2))
        (= (length (parametric-type/arguments obj1))
           (length (parametric-type/arguments obj2)))
        (every #'type/compatible?
           (parametric-type/arguments obj1)
           (parametric-type/arguments obj2))))
    ((and (type-of-kind? obj1 'type/record)
        (type-of-kind? obj2 'type/record))
     (and (= (length (record/fields obj1))
           (length (record/fields obj2)))
        (alexandria:set-equal (record/fields obj1)
                              (record/fields obj2)
                              :test
                              #'(lambda (field1 field2)
                                  (and (funcall *field/name-test*
                                              (field/name field1)
                                              (field/name field2))
                                     (type/compatible? (field/type field1)
                                                       (field/type field2)))))))
    ((and (type-of-kind? obj1 'type/function)
        (type-of-kind? obj2 'type/function))
     (and (type/compatible? (function-type/argument obj1)
                          (function-type/argument obj2))
        (type/compatible? (function-type/result obj1)
                          (function-type/result obj2))))))

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

;;; *** arrow type selector ***

(defun arrow/source-selector (arrow)
  (getf (arrow/properties arrow) :source-selector))

(defun arrow/target-selector (arrow)
  (getf (arrow/properties arrow) :target-selector))

(defparameter *type-constraint/arrow-print-function*
  #'(lambda (plist)
      (format nil "(~S -> ~S)"
              (getf plist :input-selector)
              (getf plist :output-selector))))

;;; *** module print function ***

(defparameter *type-constraint/module-print-function*
  #'(lambda (plist)
      (format nil "(~S -> ~S)"
              (getf plist :input-type)
              (getf plist :output-type))))

;;; *** type constraint ***

(defparameter *type-constraint-function*
  #'(lambda (source-node target-node arrow graph)
      (declare (ignore graph))
      (type/compatible? (record/nested-search (node/type source-node)
                                              (arrow/source-selector arrow))
                        (record/nested-search (node/type target-node)
                                              (arrow/target-selector arrow)))))
