;;;; reference.lisp

(in-package #:cl-gp)

(defconstant +purpose/reference-source+ :ref-source)
(defconstant +purpose/reference-target+ :ref-target)
(defconstant +purpose/reference+ :reference)

(defun node/reference-source? (node)
  (purpose-equal (object/purpose node) +purpose/reference-source+))

(defun node/reference-target? (node)
  (purpose-equal (object/purpose node) +purpose/reference-target+))

(defun connection/reference? (connection)
  (purpose-equal (object/purpose connection) +purpose/reference+))

(defun make-reference-connection (source-label target-label &rest args)
  (apply (alexandria:curry #'make-connection source-label target-label
                           :arrow nil
                           :purpose +purpose/reference+
                           :arrow->string-fn (constantly " REF. BY "))
         (alexandria:remove-from-plist args :arrow :purpose :arrow->string-fn)))



(defparameter *reference-relations*
  (make-structural-constraint
   :name :reference-relations
   :constraint-test-fn
   #'(lambda (source-node target-node connection graph)
       (or (not (connection/reference? connection))
          (and (node/reference-source? source-node)
             (node/reference-target? target-node)
             (null (graph/input-connections graph (list target-node)
                                            :purpose +purpose/reference+)))))))
