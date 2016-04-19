;;;; program.lisp

(in-package #:cl-gp)

;;;; В этом файле должны быть все ограничения, интерпретатор и прочие функции

;;; *** selector ***

(defparameter *tag-test* #'eql)

(defclass data-field-selector (abstract-selector)
  ((tags :accessor selector/tags
         :initarg :tags
         :initform nil)))

(defmethod selector/description-string ((selector data-field-selector))
  (let ((*print-circle* nil))
    (format nil "~:S" (selector/tags selector))))

(defun make-data-field-selector (tags-list)
  (make-instance 'data-field-selector :tags (copy-list tags-list)))

(defmethod copy-selector ((selector data-field-selector))
  (make-data-field-selector (selector/tags selector)))

(defmethod selector-equal ((selector1 data-field-selector) (selector2 data-field-selector))
  (let ((tags1 (selector/tags selector1))
        (tags2 (selector/tags selector2)))
    (and (= (length tags1) (length tags2))
       (every *tag-test* tags1 tags2))))

;;; *** graph -> s-expression convertion ***

#|
(defun graph->sexp (graph)
  #|TODO|#
  (declare (ignore graph))
  nil)
|#


#|
(defparameter *feedforward-constraint-function* (constantly t)) ; dummy
(defparameter *finite-recursion-constraint-function* (constantly t)) ; dummy
|#
