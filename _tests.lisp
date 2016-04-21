(in-package #:cl-gp)

;;; *** factorial function ***

#|
(if (= n 0)
    1
    (* n (factorial (- n 1))))
|#

(setf *functionality-modules*
   (list *name-functionality*))

(setf *functionality-info-string-function-packages*
   (list *name-info-string-function-package*))

(defparameter *boolean-type* (make-primitive-type 'boolean))
(defparameter *integer-type* (make-primitive-type 'integer))
(defparameter *number-type*
  (make-primitive-type 'number :reducibility-test-fn
                       #'(lambda (source target)
                           (declare (ignore target))
                           (if (type-name-equal )))))

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

#|
(setf *constraints-conjoint-function*
      (make-conjoint-function (list *disjoint-inputs-constraint-function*
                                    *feedforward-constraint-function*
                                    *type-constraint-function*)))

(defparameter *boolean-type* (make-primitive-type 'boolean))

(defparameter *integer-type*
  (make-primitive-type 'integer
                       :reducibility-test ;;; temporary
                       #'(lambda (type)
                           (and (type/primitive? type)
                                (type-name-equal (primitive-type/name type) 'number)))))
(defparameter *number-type*
  (make-primitive-type 'number
                       :reducibility-test
                       #'(lambda (type)
                           (and (type/primitive? type)
                                (type-name-equal (primitive-type/name type) 'integer)))))

(setf *module/info-functions-list*
      (list *name-print-function*
            (constantly " : ")
            *type-constraint/info-function*))

(setf *node/info-functions-list*
      (list *name-print-function*
            (constantly " : ")
            *type-constraint/info-function*))

(defparameter *world-node/info-functions-list*
  (list *type-constraint/world-node-info-function*))

(defparameter *factorial*
  (make-module :wn-properties
               (join-properties
                (list (make-name-property 'factorial)
                      (make-module-type-properties :input-type *integer-type*
                                                   :output-type *integer-type*)))
               :wn-events-handler-fn
               (make-sequence-function
                (list))))



(graph/add-node!
 (module/graph *factorial*)
 (make-node 1 :properties
            (join-properties
             (list (make-name-property 'if)
                   (make-type-properties :input-type (make-record
                                                      (list
                                                       (make-field 'condition *boolean-type*)
                                                       (make-field 'consequence *top-type*)
                                                       (make-field 'alternative *top-type*)))
                                         :output-type *top-type*)))
            :events-handler-fn
            (make-sequence-function
             (list #|#'(lambda (node connection graph)
              (multiple-value-bind (direction label)
              (connection/other-label connection 1)
              (if (or (direction/input? direction)
              (direction/loop? direction))
              )))|#))))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 2 :properties
            (join-properties
             (list (make-name-property 1)
                   (make-type-properties :input-type *bottom-type*
                                         :output-type *integer-type*)))))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 3 :properties
            (join-properties
             (list (make-name-property 'zerop)
                   (make-type-properties :input-type *number-type*
                                         :output-type *boolean-type*)))))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 4 :properties
            (join-properties
             (list (make-name-property '*)
                   (make-type-properties :input-type (make-record
                                                      (list
                                                       (make-field 'arg1 *number-type*)
                                                       (make-field 'arg2 *number-type*)))
                                         :output-type *number-type*)))))

(let ((recursion-node (make-node 5 :events-handler-fn
                                 (make-sequence-function
                                  (list #|#'(lambda (node connection graph)
                                   (multiple-value-bind (direction label)
                                   (connection/other-label connection 1)
                                   (if (or (direction/input? direction)
                                   (direction/loop? direction))
                                   )))|#)))))
  (graph/add-node!
   (module/graph *factorial*)
   recursion-node)
  (associate-node-with-module recursion-node *factorial*))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 6 :properties
            (join-properties
             (list (make-name-property '1-)
                   (make-type-properties :input-type *number-type*
                                         :output-type *number-type*)))))



(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) 1 *world-node-label*))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(consequence)) 2 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) *world-node-label* 3))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(condition)) 3 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(alternative)) 4 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(arg1)) *world-node-label* 4))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(arg2)) 5 4))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) 6 5))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) *world-node-label* 6))
|#
