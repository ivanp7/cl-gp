;;;; programs.lisp

(in-package #:cl-gp)

;;;; В этом файле должны быть все ограничения, интерпретатор и прочие функции

;;; *** object name ***

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(defparameter *name-constraint*
  (make-structural-constraint
   :name 'names
   :event-handler-fn-getter
   #'(lambda (kind)
       (if (kind-equal kind +kind/node+)
           #'(lambda (node event &rest args)
               (let ((connection (getf args :connection))
                     (graph (getf args :graph)))
                 (case event
                   (:on-setting-of-connection
                    (if (and (node/reference-target? node)
                           (connection/reference? connection))
                        (object/set-property-value!
                         node :name
                         (object/get-property-value
                          (graph/node graph
                                      (connection/other-node-label
                                       connection (node/label node)))
                          :name))))
                   (:on-loss-of-connection
                    (if (and (node/reference-target? node)
                           (connection/reference? connection))
                        (object/set-property-value! node :name nil))))))))
   :init-args-getter
   #'(lambda (kind)
       (if (not (kind-equal kind +kind/connection+))
           '(:name)))
   :properties-constr-fn-getter
   #'(lambda (kind)
       (if (not (kind-equal kind +kind/connection+))
           #'(lambda (&key name)
               (make-property :name name))))))

(defparameter *name-info-string-function-getter-container*
  (make-info-string-function-getter-container
   :name :name
   :info-string-fn-getter
   #'(lambda (kind)
       (if (or (kind-equal kind +kind/node+) (kind-equal kind +kind/graph+))
           #'(lambda (object)
               (let ((*print-circle* nil))
                 (format nil "{NAME ~S}"
                         (object/name object))))))))

;;; *** graph -> s-expression convertion ***

(defun graph->sexp (graph)
  (if (null (graph/node graph *world-node-id*))
      nil
      #|TODO|#))



#|
(defparameter *feedforward-constraint-function* (constantly t)) ; dummy
(defparameter *finite-recursion-constraint-function* (constantly t)) ; dummy
|#
