;;;; type.lisp

(in-package #:cl-gp)

(defconstant +type/bottom+ nil)
(defconstant +type/void+ (list 'void))
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

;;; *** tuple ***

(defun tuple/new (list-of-types)
  (case (length list-of-types)
    (0 +type/bottom+)
    (1 (first list-of-types))
    ((t) (cons 'tuple (copy-list list-of-types)))))

(defun tuple/types (tuple)
  (cdr tuple))
(defun (setf tuple/types) (new-value tuple)
  (setf (cdr tuple) new-value))

(defun type/tuple? (typ)
  (and (listp typ) (eql (first typ) 'tuple)))

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

(defun type-variable~/new (id)
  (list 'type-variable id))

(defun type-variable~/id (tvar)
  (second tvar))
(defun (setf type-variable~/id) (new-value tvar)
  (setf (second tvar) new-value))

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
         (tuple (and (= (length (tuple/types typ1))
                      (length (tuple/types typ2)))
                   (every #'identity (mapcar #'type/compatible?
                                         (tuple/types typ1)
                                         (tuple/types typ2)))))
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
