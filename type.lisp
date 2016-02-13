;;;; type.lisp

(in-package #:cl-gp)

(defconstant +type/impossible+ nil)
(defconstant +type/any+ t)
(defconstant +type/any-simple+ '(t))

;;; *** function type ***

(defun type/function-type (arguments results)
  (list (copy-list arguments) '-> (copy-list results)))

(defun type/function-type? (typ)
  (and (listp typ) (eql (second typ) '->)))

(defun type/simple? (typ)
  (not (type/function-type? typ)))

(defun function-type/arguments (ftyp)
  (first ftyp))
(defun (setf function-type/arguments) (new-value ftyp)
  (setf (first ftyp) new-value))

(defun function-type/results (ftyp)
  (third results))
(defun (setf function-type/results) (new-value ftyp)
  (setf (third results) new-value))

;;; *** type template ***

(defun type/template (possible-types)
  (cons 'or (copy-list possible-types)))

(defun type/template? (typ)
  (and (listp typ) (eql (first typ) 'or)))

(defun template/possible-types (templ)
  (rest templ))
(defun (setf template/possible-types) (new-value templ)
  (setf (rest templ) new-value))

(defun type/uncertain? (typ)
  (if (type/function-type? typ)
      (or (some #'type/uncertain? (function-type/arguments typ))
         (some #'type/uncertain? (function-type/results typ)))
      (or (equal typ +type/any+)
         (equal typ +type/any-simple+)
         (type/template? typ))))

(defun type/impossible? (typ)
  (if (type/function-type? typ)
      (or (some #'type/impossible? (function-type/arguments typ))
         (some #'type/impossible? (function-type/results typ)))
      (equal typ +type/impossible+)))
