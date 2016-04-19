;;;; reference.lisp

(in-package #:cl-gp)

(defconstant +purpose/reference-master-source+ 'ref-master-source)
(defconstant +purpose/reference-source+ 'ref-source)
(defconstant +purpose/reference-target+ 'ref-target)
(defconstant +purpose/reference+ 'reference)

(defun node/reference-master-source? (node)
  (purpose-equal (object/purpose node) +purpose/reference-master-source+))

(defun node/reference-regular-source? (node)
  (purpose-equal (object/purpose node) +purpose/reference-source+))

(defun node/reference-source? (node)
  (or (node/reference-master-source? node)
     (node/reference-regular-source? node)))

(defun node/reference-target? (node)
  (purpose-equal (object/purpose node) +purpose/reference-target+))

(defun connection/reference? (connection)
  (purpose-equal (object/purpose connection) +purpose/reference+))

(defun make-reference-master-source-node (label &rest args)
  (apply (alexandria:curry #'make-node label
                           :purpose +purpose/reference-master-source+)
         (alexandria:remove-from-plist args :purpose)))

(defun make-reference-source-node (label &rest args)
  (apply (alexandria:curry #'make-node label
                           :purpose +purpose/reference-source+)
         (alexandria:remove-from-plist args :purpose)))

(defun make-reference-target-node (label &rest args)
  (apply (alexandria:curry #'make-node label
                           :purpose +purpose/reference-target+)
         (alexandria:remove-from-plist args :purpose)))

(defun make-reference-connection (source-label target-label &rest args)
  (apply (alexandria:curry #'make-connection source-label target-label
                           :arrow nil
                           :purpose +purpose/reference+
                           :arrow->string-fn (constantly " REF. BY "))
         (alexandria:remove-from-plist args :arrow :purpose :arrow->string-fn)))



(defparameter *reference-functionality*
  (make-functionality-module
   :name 'reference-functionality
   :constraint-node-test-fn
   #'(lambda (node graph)
       (or (not (node/reference-master-source? node))
          (not (member +purpose/reference-master-source+
                (nth-value 1 (graph/all-nodes graph))
                :test *purpose-test*))))
   :constraint-connection-test-fn
   #'(lambda (source-node target-node connection graph)
       (or (not (connection/reference? connection))
          (and (node/reference-source? source-node)
             (node/reference-target? target-node)
             (null (graph/input-connections graph (list target-node)
                                            :purpose +purpose/reference+)))))))
