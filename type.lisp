;;;; type.lisp

;;; Public functions/constants/variables of the module:
#|
+type/bottom+
+type/void+
+type/top+

type/bottom?
type/void?
type/top?
type/primitive?
type/kind

field/new
field/type
field/name
record/new
record/fields
type/record?
record/recursive-field-search

parametric-type/new
parametric-type/name
parametric-type/arguments
type/parametric?

function-type/new
function-type/argument
function-type/result
type/function-type?

type/compatible?
|#

(in-package #:cl-gp)

(defconstant +type/bottom+ nil)
(alexandria:define-constant +type/void+ (list 'void) :test #'equal)
(defconstant +type/top+ t)

(defun type/bottom? (typ)
  (null typ))

(defun type/void? (typ)
  (and (listp typ) (eql (first typ) 'void)))

(defun type/top? (typ)
  (eql typ t))

(defun type/primitive? (typ)
  (and typ (symbolp typ) (not (eql typ t))))

(defun type/kind (typ)
  (cond
    ((type/bottom? typ) nil)
    ((type/top? typ) t)
    ((type/primitive? typ) 'primitive)
    (t (first typ))))

;;; *** record ***

(defun field/new (typ name)
  (cons typ name))

(defun field/type (field)
  (car field))
(defun (setf field/type) (new-value field)
  (setf (car field) new-value))

(defun field/name (field)
  (cdr field))
(defun (setf field/name) (new-value field)
  (setf (cdr field) new-value))

(defun record/new (fields)
  (case (length fields)
    (0 +type/bottom+)
    (1 (first (field/type fields)))
    (t (if (= (length fields)
              (length (remove-duplicates fields
                                         :key #'field/name
                                         :test #'eql)))
           (cons 'record (copy-list fields))
           (error "RECORD/NEW -- record cannot have fields with identical names")))))

(defun record/fields (record)
  (cdr record))
(defun (setf record/fields) (new-value record)
  (setf (cdr record) new-value))

(defun type/record? (typ)
  (and (listp typ) (eql (first typ) 'record)))

(defun record/nested-search (record names-list)
  (if (null names-list)
      record
      (if (type/record? record)
          (let ((field (find (car names-list) (record/fields record)
                             :key #'field/name :test #'eql)))
            (if field
                (record/nested-search
                 (field/type field) (cdr names-list))
                +type/bottom+))
          +type/bottom+)))

;;; *** parametric type ***

(defun parametric-type/new (name arguments)
  (if (type/primitive? name)
      (if (null arguments)
          name
          (list 'parametric name 'of (copy-list arguments)))
      (error "PARAMETRIC-TYPE/NEW -- parametric type name ~S in not a symbol" name)))

(defun parametric-type/name (typ)
  (second typ))
(defun (setf parametric-type/name) (new-value typ)
  (setf (second typ) new-value))

(defun parametric-type/arguments (typ)
  (fourth typ))
(defun (setf parametric-type/arguments) (new-value typ)
  (setf (fourth typ) new-value))

(defun type/parametric? (typ)
  (and (listp typ) (eql (first typ) 'parametric)))

;;; *** function type ***

(defun function-type/new (argument result)
  (list 'function argument '-> result))

(defun function-type/argument (ftyp)
  (second ftyp))
(defun (setf function-type/argument) (new-value ftyp)
  (setf (second ftyp) new-value))

(defun function-type/result (ftyp)
  (fourth ftyp))
(defun (setf function-type/result) (new-value ftyp)
  (setf (fourth ftyp) new-value))

(defun type/function-type? (typ)
  (and (listp typ) (eql (first typ) 'function)))

;;; *** type variable ***

#|
(defun type-variable~/new (id)
  (list 'type-variable id))

(defun type-variable~/id (tvar)
  (second tvar))
(defun (setf type-variable~/id) (new-value tvar)
  (setf (second tvar) new-value))
|#

;;; *** TODO: add type classes ***

;;; *** other ***

(defun type/compatible? (typ1 typ2)
  (cond
    ((or (type/bottom? typ1)
         (type/bottom? typ2)) nil)
    ((or (type/void? typ1)
         (type/void? typ2)) (and (type/void? typ1)
                                 (type/void? typ2)))
    ((or (type/top? typ1)
         (type/top? typ2)) t)
    ((not (eql (type/kind typ1)
               (type/kind typ2))) nil)
    (t (case (type/kind typ1)
         (primitive (eql typ1 typ2))
         (record (and (= (length (record/fields typ1))
                         (length (record/fields typ2)))
                      (alexandria:set-equal (record/fields typ1)
                                            (record/fields typ2)
                                            :test #'(lambda (f1 f2)
                                                      (and (eql (field/name f1)
                                                                (field/name f2))
                                                           (type/compatible? (field/type f1)
                                                                             (field/type f2)))))))
         (parametric (and (eql (parametric-type/name typ1)
                               (parametric-type/name typ2))
                          (= (length (parametric-type/arguments typ1))
                             (length (parametric-type/arguments typ2)))
                          (every #'identity (mapcar #'type/compatible?
                                                    (parametric-type/arguments typ1)
                                                    (parametric-type/arguments typ2)))))
         (function (and (type/compatible? (function-type/argument typ1)
                                          (function-type/argument typ2))
                        (type/compatible? (function-type/result typ1)
                                          (function-type/result typ2))))))))
