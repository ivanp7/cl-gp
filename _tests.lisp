(in-package #:cl-gp)

;;; *** factorial function ***

#|
(if (= n 0)
    1
    (* n (factorial (- n 1))))
|#

(setf *functionality-modules* nil)
(register-functionality-module! *name-functionality*)
(register-functionality-module! *strong-typing-constraint*)

(setf *info-string-function-packages* nil)
(register-info-string-function-package! *name-info-string-function-package*)
(register-info-string-function-package! *type-info-string-function-package*)

(defparameter *boolean-type* (make-primitive-type 'boolean))

(defparameter *integer-type*
  (make-primitive-type 'integer :reducibility-test-fn
                       #'(lambda (source target fn-owner)
                           (let ((other-type (if (eql fn-owner :target)
                                                 source target)))
                             (if (and (type/primitive-type? other-type)
                                    (type-name-equal (primitive-type/name other-type)
                                                     'float))
                                 (if (eql fn-owner :target) :loss t))))))

(defparameter *float-type*
  (make-primitive-type 'float :reducibility-test-fn
                       #'(lambda (source target fn-owner)
                           (let ((other-type (if (eql fn-owner :target)
                                                 source target)))
                             (if (and (type/primitive-type? other-type)
                                    (type-name-equal (primitive-type/name other-type)
                                                     'integer))
                                 (if (eql fn-owner :target) t :loss))))))

(defparameter *number-type-class*
  (make-type-class 'number #'(lambda (source target fn-owner)
                               (let ((data-type (if (eql fn-owner :target)
                                                    source target)))
                                 (if (and (type/primitive-type? data-type)
                                        (let ((name (primitive-type/name data-type)))
                                          (or (type-name-equal name 'integer)
                                             (type-name-equal name 'float))))
                                     t)))
                   (constantly nil)))

(defparameter *factorial*
  (graph/make-graph
   (list (make-reference-master-source-node 0 :name 'factorial)
         (make-node 1 :name 'if
                    :input-type (make-record
                                 (list (make-field 'condition *boolean-type*)
                                       (make-field 'consequence (make-type-variable 'csq))
                                       (make-field 'alternative (make-type-variable 'alt))))
                    :output-type (make-type-variable 'result)
                    :internal-constraint-collection
                    (make-internal-type-variable-constraint-collection
                     (make-node-internal-type-variable-constraint
                      #'(lambda (input-type output-type)
                          (nconc (mapcar #'field/type
                                         (remove-if
                                          #'(lambda (field)
                                              (field-name-equal (field/name field)
                                                                'condition))
                                          (record/fields input-type)))
                                 (list output-type))))))
         (make-node 2 :name 1 :output-type *integer-type*)
         (make-node 3 :name 'zerop
                    :input-type (make-type-variable 'input *number-type-class*)
                    :output-type *boolean-type*)
         (make-node 4 :name '*
                    :input-type (make-record
                                 (list (make-field 'arg1
                                                   (make-type-variable 'arg1
                                                                       *number-type-class*))
                                       (make-field 'arg2
                                                   (make-type-variable 'arg2
                                                                       *number-type-class*))))
                    :output-type (make-type-variable 'result *number-type-class*)
                    :internal-constraint-collection
                    (make-internal-type-variable-constraint-collection
                     (make-node-internal-type-variable-constraint
                      #'(lambda (input-type output-type)
                          (nconc (mapcar #'field/type (record/fields input-type))
                                 (list output-type)))
                      #'(lambda (type-variables-list setter)
                          (let ((arg1 (find 'arg1 type-variables-list
                                            :key #'type-variable/name))
                                (arg2 (find 'arg1 type-variables-list
                                            :key #'type-variable/name))
                                (result (find 'result type-variables-list
                                              :key #'type-variable/name)))
                            (cond
                              ((and (type-entity/has-associated-entity? result)
                                  (not (type-entity/type-variable?
                                      (type-entity/actual-entity result)))
                                  (type-name-equal (primitive-type/name
                                                    (type-entity/actual-entity result))
                                                   ...)))))))))
         (make-reference-target-node 5)
         (make-node 6 :name '1-
                    :input-type (make-type-variable 'input *number-type-class*)
                    :output-type (make-type-variable 'output *number-type-class*)
                    :internal-constraint-collection
                    (make-internal-type-variable-constraint-collection
                     (make-node-internal-type-variable-constraint
                      #'(lambda (input-type output-type)
                          (list input-type output-type))))))
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
                                                 (make-data-selector '(arg1))))
         (make-connection 5 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(arg2))))
         (make-connection 6 5 :arrow (make-arrow))
         (make-connection 0 6 :arrow (make-arrow)))))

#|
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
                                                 (make-data-selector '(arg1))))
         (make-connection 5 4 :arrow (make-arrow :target-selector
                                                 (make-data-selector '(arg2))))
         (make-connection 6 5 :arrow (make-arrow))
         (make-connection 0 6 :arrow (make-arrow)))))
|#
