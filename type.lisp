;;;; type.lisp

(in-package #:cl-gp)

(defconstant +type/impossible+ nil)
(defconstant +type/void+ (list 'void))

(defun type/impossible? (typ)
  (null typ))

(defun type/void? (typ)
  (and (listp typ) (eql (first typ) 'void)))

(defun type/simple? (typ)
  (and typ (symbolp typ)))

(defun type/kind (typ)
  (cond
    ((type/impossible? typ) nil)
    ((type/simple? typ) 'simple)
    (t (first typ))))

;;; *** tuple ***

(defun tuple/new (list-of-types)
  (if (notany #'type/void? list-of-types)
      (if (some #'type/impossible? list-of-types)
          +type/impossible+
          (case (length list-of-types)
            (0 +type/void+)
            (1 (first list-of-types))
            ((t) (cons 'tuple (copy-list list-of-types)))))
      (error "TUPLE/NEW -- tuple cannot contain void type")))

(defun tuple/types (tuple)
  (cdr tuple))
(defun (setf tuple/types) (new-value tuple)
  (setf (cdr tuple) new-value))

(defun type/tuple? (typ)
  (and (listp typ) (eql (first typ) 'tuple)))

;;; *** parametric type ***

(defun parametric-type/new (name arguments)
  (if (type/simple? name)
      (if (zerop (length arguments))
          name
          (if (notany #'type/void? arguments)
              (if (some #'type/impossible? arguments)
                  +type/impossible+
                  (list 'parametric name 'of (copy-list arguments)))
              (error "PARAMETRIC-TYPE/NEW -- parametric type cannot have an argument of void type")))
      (error "PARAMETRIC-TYPE/NEW -- parametric type name ~S in not a true symbol" name)))

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
  (if (or (type/impossible? argument)
         (type/impossible? result))
      +type/impossible+
      (list 'function argument '-> result)))

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

;;; *** type template ***

(defconstant +template/any+ (list 'template 'any))
(defconstant +template/simple+ (list 'template 'simple))

(defun template/enumeration (list-of-types)
  (let ((list-of-types (remove +type/impossible+ list-of-types)))
    (case (length list-of-types)
      (0 +type/impossible+)
      (1 (first list-of-types))
      ((t) (list 'template 'enum list-of-types)))))

(defun template-enumeration/types (typ)
  (third typ))
(defun (setf template-enumeration/types) (new-value typ)
  (setf (third typ) new-value))

(defun template/union (temp1 temp2)
  +type/impossible+)

(defun template/intersection (temp1 temp2)
  +type/impossible+)

(defun template/difference (temp1 temp2)
  +type/impossible+)

(defun type/template? (typ)
  (ccase (type/kind typ)
    (nil nil)
    (simple nil)
    (tuple (some #'type/template? (tuple/types typ)))
    (parametric (some #'type/template? (parametric-type/arguments typ)))
    (function (or (type/template? (function-type/argument typ))
                 (type/template? (function-type/result typ))))
    (template t)))

;;; *** other ***

(defun type/compatible? (typ1 typ2)
  (cond
    ((or (type/impossible? typ1)
        (type/impossible? typ2)) nil)
    ((and (not (type/template? typ1))
        (not (type/template? typ2))) (equal typ1 typ2))
    (t +type/impossible+)))
