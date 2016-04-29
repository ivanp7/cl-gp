;;;; name.lisp

(in-package #:cl-gp)

;;; *** object name ***

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(flet ((bind-target-name! (node target)
         (let ((node-name-property (properties/get-property
                                    (object/properties node) :name))
               (target-name-property (properties/get-property
                                      (object/properties target) :name)))
           (function-collection/add-function!
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality target
            #'(lambda (value)
             (property/force-value! target-name-property value))
            t (list (property/value node-name-property)))))
       (unbind-target-name! (node target)
         (let ((node-name-property (properties/get-property
                                    (object/properties node) :name)))
           (function-collection/call-function
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality target (list nil))
           (function-collection/delete-function!
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality target)))
       (unbind-all-bound-names! (node)
         (let ((node-name-property (properties/get-property
                                    (object/properties node) :name)))
           (function-collection/call-functions
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality (list nil))
           (function-collection/delete-functions!
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality))))

  (defparameter *name-functionality*
    (make-functionality-module
     :name 'name-functionality
     :dependencies (list *reference-functionality*)
     :event-handler-fn-getter
     #'(lambda (object-class object)
         (declare (ignore object))
         (if (eql object-class 'object/node)
             #'(lambda (node event &key connection other-node graph)
                 (case event
                   (:on-addition-to-graph
                    (when (node/reference-master-source? node)
                      (bind-target-name! node graph)))
                   (:on-deletion-from-graph
                    (unbind-all-bound-names! node))
                   (:on-setting-of-connection
                    (when (and (node/reference-source? node)
                             (connection/reference? connection))
                      (bind-target-name! node other-node)))
                   (:on-loss-of-connection
                    (when (and (node/reference-source? node)
                             (connection/reference? connection))
                      (unbind-target-name! node other-node)))))))
     :properties-constr-fn-getter
     #'(lambda (object-class purpose)
         (if (or (eql object-class 'object/node)
                (eql object-class 'object/graph))
             (let ((name-settable-p
                    (and (eql object-class 'object/node)
                       (not (purpose-equal purpose +purpose/reference-target+)))))
               (values #'(lambda (present-properties &key (name nil name-supplied-p))
                           (let ((name-property (getf present-properties :name)))
                             (if (null name-property)
                                 (make-property :name name
                                                :value-copy-fn
                                                (if name-settable-p
                                                    #'identity
                                                    (constantly nil))
                                                :value-setting-fn
                                                (if name-settable-p
                                                    +property/writable+
                                                    +property/read-only+))
                                 (when name-supplied-p
                                   (setf (property/value name-property) name)
                                   nil))))
                       (if name-settable-p
                           '(:name))
                       '(:name))))))))

(defparameter *name-info-string-function-package*
  (make-info-string-function-package
   :name :name
   :info-string-fn-getter
   #'(lambda (object-class object)
       (declare (ignore object))
       (if (or (eql object-class 'object/node)
              (eql object-class 'object/graph))
           #'(lambda (object)
               (let ((*print-circle* nil))
                 (format nil "NAME: ~S"
                         (object/name object))))))))
