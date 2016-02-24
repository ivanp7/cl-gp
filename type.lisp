;;;; type.lisp

;;; Public symbols of the file:
#|
type/abstract
type-object?
type/kind
type-of-kind?
kind=

*type/name-test*

type/class
type-class/name
type-class/type-test
make-type-class
type/class?

type/top
type/top?
+type/top+

type/void
type/void?
+type/void+

type/bottom
type/bottom?
+type/bottom+

type/parametric
parametric-type/name
parametric-type/arguments
make-parametric-type
type/parametric?

type/primitive
primitive-type/name
make-primitive-type
type/primitive?

*field/name-test*
field
field/name
field/type
make-field

type/record
record/fields
make-record
type/record?
record/nested-search

type/function
function-type/argument
function-type/result
make-function-type
type/function?

type/compatible?
|#

(in-package #:cl-gp)



(defclass type/abstract ()
  ())

(defmethod initialize-instance :after ((instance type/abstract) &key)
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



(defvar *type/name-test* #'eql)

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

(declaim (ftype function make-primitive))

(defun make-parametric-type (name arguments-list)
  (if (zerop (length arguments-list))
      (make-primitive-type name)
      (make-instance 'type/parametric :name name :arguments arguments-list)))

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

(defun make-primitive-type (name)
  (make-instance 'type/primitive :name name))

(defun type/primitive? (type)
  (typep type 'type/primitive))

;;; *** field&record ***

(defvar *field/name-test* #'eql)

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
      (format st "RECORD: ~S fields: ~S" (length fields) fields))))

(defun make-record (fields-list)
  (case (length fields-list)
    (0 +type/bottom+)
    (1 (field/type (first fields-list)))
    (t (if (= (length fields-list)
              (length (remove-duplicates fields-list
                                         :key #'field/name
                                         :test *field/name-test*)))
           (make-instance 'type/record :fields fields-list)
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

(defun make-function-type (argument result)
  (make-instance 'type/function :argument argument :result result))

(defun type/function? (type)
  (typep type 'type/function))

;;; *** other ***

(defun type/compatible? (typ1 typ2)
  (cond
    ((and (type-of-kind? typ1 'type/class)
        (type-of-kind? typ2 'type/class))
     (funcall *type/name-test* (type-class/name typ1) (type-class/name typ2)))
    ((or (type-of-kind? typ1 'type/class)
        (type-of-kind? typ2 'type/class))
     (if (type-of-kind? typ1 'type/class)
         (funcall (type-class/type-test typ1) typ2)
         (funcall (type-class/type-test typ2) typ1)))
    ((and (type-of-kind? typ1 'type/parametric)
        (type-of-kind? typ2 'type/parametric))
     (and (funcall *type/name-test*
                 (parametric-type/name typ1)
                 (parametric-type/name typ2))
        (= (length (parametric-type/arguments typ1))
           (length (parametric-type/arguments typ2)))
        (every #'type/compatible?
           (parametric-type/arguments typ1)
           (parametric-type/arguments typ2))))
    ((and (type-of-kind? typ1 'type/record)
        (type-of-kind? typ2 'type/record))
     (and (= (length (record/fields typ1))
           (length (record/fields typ2)))
        (alexandria:set-equal (record/fields typ1)
                              (record/fields typ2)
                              :test
                              #'(lambda (field1 field2)
                                  (and (funcall *field/name-test*
                                              (field/name field1)
                                              (field/name field2))
                                     (type/compatible? (field/type field1)
                                                       (field/type field2)))))))
    ((and (type-of-kind? typ1 'type/function)
        (type-of-kind? typ2 'type/function))
     (and (type/compatible? (function-type/argument typ1)
                          (function-type/argument typ2))
        (type/compatible? (function-type/result typ1)
                          (function-type/result typ2))))))
