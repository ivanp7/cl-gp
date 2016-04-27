(in-package #:cl-gp)

;;; *** factorial function ***

#|
(if (= n 0)
    1
    (* n (factorial (- n 1))))
|#

(setf *functionality-modules* nil)
(register-functionality-module! *name-functionality*)
;; (register-functionality-module! *strong-typing-constraint*)

(setf *info-string-function-packages* nil)
(register-info-string-function-package! *name-info-string-function-package*)
;; (register-info-string-function-package! *type-info-string-function-package*)

(defparameter *factorial*
  (graph/make-graph
   (list (make-reference-master-source-node 0 :name 'factorial)
         (make-node 1 :name 'if)
         (make-node 2 :name 1)
         (make-node 3 :name 'zerop)
         (make-node 4 :name '*)
         (make-reference-target-node 5)
         (make-node 6 :name '1-))
   (list (make-reference-connection 0 5)
         (make-connection 1 0 :arrow (make-arrow))
         (make-connection 2 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(consequence))))
         (make-connection 3 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(condition))))
         (make-connection 0 3 :arrow (make-arrow))
         (make-connection 4 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(alternative))))
         (make-connection 0 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(value1))))
         (make-connection 5 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(value2))))
         (make-connection 6 5 :arrow (make-arrow))
         (make-connection 0 6 :arrow (make-arrow)))))

#|
(defparameter *boolean-type* (make-primitive-type 'boolean))
(defparameter *integer-type* (make-primitive-type 'integer))
(defparameter *number-type*
  (make-primitive-type 'number :reducibility-test-fn
                       #'(lambda (source target fn-owner)
                           (declare (ignore fn-owner))
                           (if (and (type/primitive-type? source)
                                    (type/primitive-type? target))
                               (cond
                                 ((and (type-name-equal (primitive-type/name source)
                                                        'integer)
                                       (type-name-equal (primitive-type/name target)
                                                        'number))
                                  t)
                                 ((and (type-name-equal (primitive-type/name source)
                                                        'number)
                                       (type-name-equal (primitive-type/name target)
                                                        'integer))
                                  :loss))))))

(defparameter *factorial*
  (graph/make-graph
   (list (make-reference-master-source-node 0 :name 'factorial
                                            :input-type (make-type-variable :input)
                                            :output-type (make-type-variable :output))
         (make-node 1 :name 'if
                    :input-type (make-record
                                 (list (make-field 'condition *boolean-type*)
                                       (make-field 'consequence (make-type-variable :csq))
                                       (make-field 'alternative (make-type-variable :alt))))
                    :output-type (make-type-variable :result)
                    :internal-type-variable-constraints
                    (list (make-node-internal-type-variable-constraint
                           #'(lambda (input-type output-type)
                               (list (field/type (find-if #'(lambda (field)
                                                              (eql (field/name field)
                                                                   'consequence))
                                                          (record/fields input-type)))
                                     (field/type (find-if #'(lambda (field)
                                                              (eql (field/name field)
                                                                   'alternative))
                                                          (record/fields input-type)))
                                     output-type)))))
         (make-node 2 :name 1 :output-type *integer-type*)
         (make-node 3 :name 'zerop :input-type *number-type* :output-type *boolean-type*)
         (make-node 4 :name '*
                    :input-type (make-record
                                 (list (make-field 'value1 *number-type*)
                                       (make-field 'value2 *number-type*)))
                    :output-type *number-type*)
         (make-reference-target-node 5)
         (make-node 6 :name '1- :input-type *number-type* :output-type *number-type*))
   (list (make-reference-connection 0 5)
         (make-connection 1 0 :arrow (make-arrow))
         (make-connection 2 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(consequence))))
         (make-connection 3 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(condition))))
         (make-connection 0 3 :arrow (make-arrow))
         (make-connection 4 1 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(alternative))))
         (make-connection 0 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(value1))))
         (make-connection 5 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(value2))))
         (make-connection 6 5 :arrow (make-arrow))
         (make-connection 0 6 :arrow (make-arrow)))))
|#
