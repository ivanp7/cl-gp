;;;; interpreter.lisp

;;; Public functions/constants/variables of the module:
#|
|#

(in-package #:cl-gp)

;;; *** objects names ***

(defparameter *name-print-function*
  #'(lambda (plist)
      (format nil "~S" (getf plist :name))))

(defun node/name (node)
  (getf (node/properties node) :name))

(defun module/name (module)
  (getf (module/properties module) :name))

(defun make-name-property (name)
  (make-properties :name name))

;;; *** graph -> s-expression convertion ***

(defun graph->sexp (graph)
  (if (null (graph/node graph *world-node-id*))
      nil
      #|TODO|#))

;;; *** standard node features ***
#|
(defconstant +behaviour/value+ nil)
(defconstant +behaviour/function-call+ :function)

(defun make-node-properties (&key name behaviour (type +type/bottom+) object)
  (case behaviour
    (+behaviour/function-call+
     (unless (type/function? type)
       (error "OBJECT/NODE -- 'function call' node must have function type")))
    (+behaviour/value+ nil)
    (t (error "OBJECT/NODE -- invalid behaviour is specified")))
  (list :name name
        :behaviour behaviour
        :type type
        :object object))

(defun node/behaviour (node)
  (getf (node/properties node) :behaviour))

(defun node/object (node)
  (getf (node/properties node) :object))





(defun node/value? (node)
  (eql (node/behaviour node) +behaviour/value+))

(defun node/call? (node)
  (eql (node/behaviour node) +behaviour/function-call+))
|#
