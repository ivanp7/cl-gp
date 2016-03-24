(in-package #:cl-gp)

;;; *** factorial function ***

#|
(if (= n 0)
    1
    (* n (factorial (- n 1))))
|#

(setf *constraints-conjoint-function*
   (make-conjoint-function (list *feedforward-constraint-function*
                                 *type-constraint-function*)))

(defparameter *boolean-type* (make-primitive-type 'boolean))

(defparameter *integer-type* (make-primitive-type 'integer))
(defparameter *number-type*
  (make-primitive-type 'number
                       :reducibility-test
                       #'(lambda (type)
                           (and (type/primitive? type)
                              (type-name-equal (primitive-type/name type) 'integer)))))

(setf *module/print-functions-list*
   (list *name-print-function*
         (constantly " : ")
         *type-constraint/module-print-function*))

(setf *node/print-functions-list*
   (list *name-print-function*
         (constantly " : ")
         *type-constraint/node-print-function*))

(defparameter *world-node/print-functions-list*
  (list *type-constraint/world-node-print-function*))

(defparameter *factorial*
  (make-module :wn-properties
               (join-properties
                (list (make-name-property 'factorial)
                      (make-module-type-properties :input-type *integer-type*
                                                   :output-type *integer-type*)))))



(graph/add-node!
 (module/graph *factorial*)
 (make-node 1 :properties
            (make-properties
             :name 'if
             :input-type (make-record
                          (list
                           (make-field 'condition *boolean-type*)
                           (make-field 'consequence *top-type*)
                           (make-field 'alternative *top-type*)))
             :output-type *top-type*)
            :setting-of-connection-fn
            (make-sequence-function
             (list #|#'(lambda (node connection graph)
              (multiple-value-bind (direction id)
              (connection/other-id connection 1)
              (if (or (direction/input? direction)
              (direction/loop? direction))
              )))|#))
            :loss-of-connection-fn
            (make-sequence-function
             (list #|#'(lambda (node connection graph)
              )|#))))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 2 :properties
            (make-properties
             :name 1
             :input-type *bottom-type*
             :output-type *integer-type*)))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 3 :properties
            (make-properties
             :name 'zerop
             :input-type *number-type*
             :output-type *boolean-type*)))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 4 :properties
            (make-properties
             :name '*
             :input-type (make-record
                          (list
                           (make-field 'arg1 *number-type*)
                           (make-field 'arg2 *number-type*)))
             :output-type *number-type*)))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 5 :properties
            (make-properties
             :name 'factorial
             :input-type *integer-type*
             :output-type *integer-type*)
            :setting-of-connection-fn
            (make-sequence-function
             (list #|#'(lambda (node connection graph)
              (multiple-value-bind (direction id)
              (connection/other-id connection 1)
              (if (or (direction/input? direction)
              (direction/loop? direction))
              )))|#))
            :loss-of-connection-fn
            (make-sequence-function
             (list #|#'(lambda (node connection graph)
              )|#))))

(graph/add-node!
 (module/graph *factorial*)
 (make-node 6 :properties
            (make-properties
             :name '1-
             :input-type *number-type*
             :output-type *number-type*)))



(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) 1 *world-node-id*))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(consequence)) 2 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) *world-node-id* 3))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(condition)) 3 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(alternative)) 4 1))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(arg1)) *world-node-id* 4))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow :target-selector '(arg2)) 5 4))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) 6 5))

(graph/connect!
 (module/graph *factorial*)
 (make-connection (make-arrow) *world-node-id* 6))
