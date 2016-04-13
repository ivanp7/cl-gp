;;;; programs.lisp

(in-package #:cl-gp)

;;;; В этом файле должны быть все ограничения, интерпретатор и прочие функции

;;; *** object name ***

(defun make-name-property-list (name)
  (list (make-property :name name)))

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(defparameter *name-info-string-functions-package*
  (make-info-string-functions-package
   :name :name
   :common-info-string-fn
   #'(lambda (object)
       (let ((*print-circle* nil))
         (format nil "{NAME ~S}"
                 (object/name object))))))

;;; *** graph -> s-expression convertion ***

(defun graph->sexp (graph)
  (if (null (graph/node graph *world-node-id*))
      nil
      #|TODO|#))



#|
(defparameter *feedforward-constraint-function* (constantly t)) ; dummy
(defparameter *finite-recursion-constraint-function* (constantly t)) ; dummy
|#
