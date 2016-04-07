;;;; interpreter.lisp

(in-package #:cl-gp)

;;; *** object name ***

(defun make-name-property-list (name)
  (list (make-property :name name)))

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(defparameter *name-info-function*
  #'(lambda (object)
      (format nil "~S" (object/name object))))

;;; *** graph -> s-expression convertion ***

(defun graph->sexp (graph)
  (if (null (graph/node graph *world-node-id*))
      nil
      #|TODO|#))
