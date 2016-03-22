(in-package #:cl-gp)

;;; *** factorial function ***

#|
(if (= n 0)
    1
    (* n (factorial (- n 1))))
|#

(defparameter *boolean-type* (make-primitive-type 'boolean))
(defparameter *integer-type* (make-primitive-type 'integer))
(defparameter *greater-integer-type*
  (make-primitive-type 'greater-integer
                       :reducibility-test
                       #'(lambda (type)
                           (and (type/primitive? type)
                              (type-name-equal (type/name type) 'integer)))))

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

(defparameter *arrow/print-functions-list*
  (list *type-constraint/arrow-print-function*))

(defparameter *factorial*
  (make-module :wn-properties
               (join-properties
                (list (make-name-property 'factorial)
                      (make-module-type-properties :input-type *integer-type*
                                                   :output-type *greater-integer-type*)))))


(graph/add-node! (module/graph *factorial*)
                 (make-node 1 :properties
                            (make-properties
                             :name 'if
                             :input-type (make-record
                                          (list
                                           (make-field 'condition *boolean-type*)
                                           (make-field 'consequence +top-type+)
                                           (make-field 'alternative +top-type+)))
                             :output-type +top-type+)
                            :setting-of-connection-fn
                            #'(lambda (node connection graph)
                                (multiple-value-bind (direction id)
                                    (connection/other-id connection 1)
                                  (if (or (direction/input? direction)
                                         (direction/loop? direction))
                                      )))
                            :loss-of-connection-fn
                            #'(lambda (node connection graph)
                                )))

(print (module/add-node! *factorial* (node/new-input 'n 'integer))) ; 1
(print (module/add-node! *factorial* (node/new-output 'factorial 'integer))) ; 2

(print (module/add-node! *factorial*
                         (node/new-primitive
                          'if (record/new
                               (list
                                (field/new 'boolean 'condition)
                                (field/new +type/top+ 'consequence)
                                (field/new +type/top+ 'alternative)))
                          'integer))) ; 3
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 3)
                                        (node-socket/new 2)) 0))

(print (module/add-node! *factorial*
                         (node/new-primitive
                          '= (record/new
                              (list
                               (field/new 'integer 'arg1)
                               (field/new 'integer 'arg2)))
                          'boolean))) ; 4
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 4)
                                        (node-socket/new 3 '(condition))) 0))

(print (module/add-node! *factorial* (node/new-value 0 'integer))) ; 5
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 5)
                                        (node-socket/new 4 '(arg2))) 0))
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 1)
                                        (node-socket/new 4 '(arg1))) 0))

(print (module/add-node! *factorial* (node/new-value 1 'integer))) ; 6
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 6)
                                        (node-socket/new 3 '(consequence))) 0))

(print (module/add-node! *factorial*
                         (node/new-primitive
                          '* (record/new
                              (list
                               (field/new 'integer 'arg1)
                               (field/new 'integer 'arg2)))
                          'integer))) ; 7
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 7)
                                        (node-socket/new 3 '(alternative))) 0))

(print (module/add-node! *factorial* (node/new-module 'factorial 'integer 'integer))) ; 8
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 1)
                                        (node-socket/new 7 '(arg1))) 0))
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 8)
                                        (node-socket/new 7 '(arg2))) 0))

(print (module/add-node! *factorial*
                         (node/new-primitive
                          '- (record/new
                              (list
                               (field/new 'integer 'arg1)
                               (field/new 'integer 'arg2)))
                          'integer))) ; 9
(print (module/add-node! *factorial* (node/new-value 1 'integer))) ; 10
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 9)
                                        (node-socket/new 8)) 0))
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 1)
                                        (node-socket/new 9 '(arg1))) 0))
(print (module/connect! *factorial*
                        (connection/new (node-socket/new 10)
                                        (node-socket/new 9 '(arg2))) 0))

(princ "------------------")

(print (module/all-nodes *factorial*))
