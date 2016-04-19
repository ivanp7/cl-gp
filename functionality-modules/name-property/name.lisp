;;;; name.lisp

(in-package #:cl-gp)

;;; *** object name ***

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(defparameter *name-functionality*
  (make-functionality-module
   :name 'name-functionality
   :event-handler-fn-getter
   #'(lambda (kind)
       (if (kind-equal kind +kind/node+)
           #'(lambda (node event &rest args)
               (let ((connection (getf args :connection))
                     (graph (getf args :graph)))
                 (case event
                   (:on-addition-to-graph
                    (when (node/reference-master-source? node)
                      (let ((node-name-property (properties/get-property
                                                 (object/properties node) :name))
                            (graph-name-property (properties/get-property
                                                  (object/properties graph) :name)))
                        (property/register-value-setting-event-function!
                         node-name-property 'name-functionality nil
                         #'(lambda (value)
                             (setf (property/value graph-name-property)
                                value))
                         (property/value node-name-property)))))
                   (:on-deletion-from-graph
                    (let ((node-name-property (properties/get-property
                                               (object/properties node) :name)))
                      (property/call-value-setting-event-functions
                       node-name-property 'name-functionality nil)
                      (property/unregister-value-setting-event-functions!
                       node-name-property 'name-functionality)))
                   (:on-setting-of-connection
                    (when (and (node/reference-source? node)
                             (connection/reference? connection))
                      (let* ((other-node (graph/node graph
                                                     (connection/other-node-label
                                                      connection (node/label node))))
                             (node-name-property (properties/get-property
                                                  (object/properties node)
                                                  :name))
                             (other-node-name-property (properties/get-property
                                                        (object/properties other-node)
                                                        :name)))
                        (property/register-value-setting-event-function!
                         node-name-property 'name-functionality other-node
                         #'(lambda (value)
                             (setf (property/value
                                 other-node-name-property)
                                value))
                         (property/value node-name-property)))))
                   (:on-loss-of-connection
                    (when (and (node/reference-source? node)
                             (connection/reference? connection))
                      (let* ((other-node (graph/node graph
                                                     (connection/other-node-label
                                                      connection (node/label node))))
                             (node-name-property (properties/get-property
                                                  (object/properties node) :name)))
                        (property/call-value-setting-event-function
                         node-name-property 'name-functionality other-node nil)
                        (property/unregister-value-setting-event-function!
                         node-name-property 'name-functionality other-node)))))))))
   :init-args-getter
   #'(lambda (kind)
       (if (not (kind-equal kind +kind/connection+))
           '(:name)))
   :properties-constr-fn-getter
   #'(lambda (kind)
       (if (not (kind-equal kind +kind/connection+))
           #'(lambda (&key name)
               (make-property :name name))))))

(defparameter *name-info-string-function-package*
  (make-functionality-info-string-function-package
   :name :name
   :info-string-fn-getter
   #'(lambda (kind)
       (if (or (kind-equal kind +kind/node+)
              (kind-equal kind +kind/graph+))
           #'(lambda (object)
               (let ((*print-circle* nil))
                 (format nil "{NAME ~S}"
                         (object/name object))))))))
