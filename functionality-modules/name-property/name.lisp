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
         (let* ((node-name-property (properties/get-property
                                     (object/properties node) :name)))
           (function-collection/call-function
            (property/value-setting-event-handler-collection node-name-property)
            'name-functionality target nil)
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
     :dependencies-register-fn
     #'(lambda ()
         (register-functionality-module! *reference-functionality*))
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
             (let ((name-not-writable
                    (or (eql object-class 'object/graph)
                       (purpose-equal purpose +purpose/reference-target+))))
               (values #'(lambda (&key name)
                           (make-property :name name
                                          :value-setting-fn
                                          (if name-not-writable
                                              +property/read-only+
                                              +property/writable+)))
                       (unless name-not-writable
                         '(:name)))))))))

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
