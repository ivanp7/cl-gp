;;;; constraint.lisp

(in-package #:cl-gp)

;; реализовать параллельные значения на коннекторах в системе ограничений

;;; *** connection CPS constraint ***

(defclass cps-constraint/connection (cps/abstract-constraint)
  ((source-type-entity :initarg :source)
   (target-type-entity :initarg :target)))

(defmethod cps-constraint/establish ((this cps-constraint/connection) &rest args)
  (declare (ignore args))
  (with-slots (source-type-entity target-type-entity) this
    (cps-connector/connect! (slot-value source-type-entity 'cps-connector) this)
    (cps-connector/connect! (slot-value target-type-entity 'cps-connector) this)))

(defmethod cps-constraint/abolish ((this cps-constraint/connection))
  (with-slots (source-type-entity target-type-entity) this
    (cps-connector/disconnect! (slot-value source-type-entity 'cps-connector) this)
    (cps-connector/disconnect! (slot-value target-type-entity 'cps-connector) this)))

(defmethod initialize-instance :after ((this cps-constraint/connection) &key)
  (cps-constraint/establish this))

(defun ~make-connection-cps-constraint (source-type-entity target-type-entity)
  (make-instance 'cps-constraint/connection
                 :source source-type-entity
                 :target target-type-entity))

(defmethod cps-constraint/inform-about-value ((this cps-constraint/connection))
  (with-slots (source-type-entity target-type-entity) this
    (cond ((type-entity/has-associated-entity? source-type-entity)
           (type-entity/associate-entity! target-type-entity
                                          (type-entity/associated-entity
                                           source-type-entity) this))
          ((type-entity/has-associated-entity? target-type-entity)
           (type-entity/associate-entity! source-type-entity
                                          (type-entity/associated-entity
                                           target-type-entity) this)))))

(defmethod cps-constraint/inform-about-no-value ((this cps-constraint/connection))
  (with-slots (source-type-entity target-type-entity) this
    (type-entity/unassociate-entity! source-type-entity this)
    (type-entity/unassociate-entity! target-type-entity this)
    (cps-constraint/inform-about-value this)))

;;; *** node CPS constraint ***

(defclass cps-constraint/node (cps/abstract-constraint)
  ((type-variables-list :initform nil)
   (type-variables-list-getter :initarg :type-variables-list-getter
                               :initform (constantly nil))
   (cps-constraint-fn :initarg :cps-constraint-fn
                      :initform nil)))

(defmethod cps-constraint/establish ((this cps-constraint/node) &rest args)
  (with-slots (type-variables-list type-variables-list-getter) this
    (let ((input-type (getf args :input-type))
          (output-type (getf args :output-type)))
      (setf type-variables-list (funcall type-variables-list-getter input-type output-type)))
    (dolist (typevar type-variables-list)
      (cps-connector/connect! (slot-value typevar 'cps-connector) this))))

(defmethod cps-constraint/abolish ((this cps-constraint/node))
  (with-slots (type-variables-list) this
    (dolist (typevar type-variables-list)
      (cps-connector/disconnect! (slot-value typevar 'cps-connector) this))
    (setf type-variables-list nil)))

(defmethod initialize-instance :after ((this cps-constraint/node) &key)
  (with-slots (type-variables-list-getter cps-constraint-fn) this
    (unless cps-constraint-fn
      (setf cps-constraint-fn
         #'(lambda (type-variables-list setter)
             (let ((assoc-typevar (find-if #'type-entity/has-associated-entity?
                                           type-variables-list)))
               (dolist (typevar type-variables-list)
                 (type-entity/associate-entity! typevar
                                                (type-entity/associated-entity assoc-typevar)
                                                setter))))))))

(defmethod cps-constraint/inform-about-value ((this cps-constraint/node))
  (with-slots (type-variables-list) this
    (funcall (slot-value this 'cps-constraint-fn) type-variables-list this)))

(defmethod cps-constraint/inform-about-no-value ((this cps-constraint/node))
  (with-slots (type-variables-list) this
    (dolist (typevar type-variables-list)
      (type-entity/unassociate-entity! typevar this))
    (cps-constraint/inform-about-value this)))



(defun make-node-internal-type-variable-constraint (type-variables-list-getter
                                                    &optional cps-constraint-fn)
  (make-instance 'cps-constraint/node
                 :type-variables-list-getter type-variables-list-getter
                 :cps-constraint-fn cps-constraint-fn))

(defun copy-node-internal-type-variable-constraint (constraint)
  (with-slots (type-variables-list-getter cps-constraint-fn) constraint
    (make-node-internal-type-variable-constraint
     type-variables-list-getter cps-constraint-fn)))

(defmethod copy-cps-constraint ((cps-constraint cps-constraint/node))
  (copy-node-internal-type-variable-constraint cps-constraint))

;;; *** object property readers ***

(defun object/input-type (object)
  (object/get-property-value object :input-type (bottom-type)))

(defun object/output-type (object)
  (object/get-property-value object :output-type (bottom-type)))

(defun object/type (object direction)
  (cond
    ((direction/input? direction) (object/input-type object))
    ((direction/output? direction) (object/output-type object))
    (t (error "OBJECT/TYPE -- incorrect direction ~S is supplied" direction))))

(defun object/internal-type-variable-constraints (object)
  (object/get-property-value object :internal-type-variable-constraints))

;;; ******************
;;; *** constraint ***
;;; ******************

(defparameter *strong-typing-enabled* t)
(defparameter *strong-typing-allow-lossy-connections* nil)

(defparameter *strong-typing-constraint*
  (make-functionality-module
   :name 'strong-typing
   :dependencies (list *reference-functionality*)
   :constraint-fn-getter
   #'(lambda (object-class object)
       (declare (ignore object))
       (if (eql object-class 'object/connection)
           #'(lambda (connection graph source-node target-node)
               (declare (ignore graph))
               (or (not *strong-typing-enabled*)
                  (let ((source-type-entity
                         (type/recursive-component-type-selection
                          (object/output-type source-node)
                          (arrow/source-selector (connection/arrow connection))))
                        (target-type-entity
                         (type/recursive-component-type-selection
                          (object/input-type target-node)
                          (arrow/target-selector (connection/arrow connection)))))
                    (let ((result (type-entity/reducible? source-type-entity
                                                          target-type-entity)))
                      (cond
                        ((not result) nil)
                        ((eql result :loss) *strong-typing-allow-lossy-connections*)
                        ((eql result t) t)
                        (t t))))))))
   :event-handler-fn-getter
   #'(lambda (object-class object)
       )
   :properties-constr-fn-getter
   #'(lambda (object-class purpose)
       (let ((type-settable-p (and (eql object-class 'object/node)
                                 (not (purpose-equal purpose +purpose/reference-target+)))))
         (values #'(lambda (present-properties
                       &key (input-type (bottom-type) input-type-supplied-p)
                         (output-type (bottom-type) output-type-supplied-p)
                         (internal-type-variable-constraints
                          nil internal-constraints-supplied-p))
                     (nconc
                      (let ((input-type-property (getf present-properties :input-type)))
                        (if (null input-type-property)
                            (list (make-property :input-type input-type
                                                 :value-copy-fn
                                                 (if type-settable-p
                                                     #'copy-type-entity
                                                     (constantly (bottom-type)))
                                                 :value-setting-fn
                                                 (if type-settable-p
                                                     +property/writable+
                                                     +property/read-only+)))
                            (when input-type-supplied-p
                              (setf (property/value input-type-property) input-type)
                              nil)))
                      (let ((output-type-property (getf present-properties :output-type)))
                        (if (null output-type-property)
                            (list (make-property :output-type output-type
                                                 :value-copy-fn
                                                 (if type-settable-p
                                                     #'copy-type-entity
                                                     (constantly (bottom-type)))
                                                 :value-setting-fn
                                                 (if type-settable-p
                                                     +property/writable+
                                                     +property/read-only+)))
                            (when output-type-supplied-p
                              (setf (property/value output-type-property) output-type)
                              nil)))
                      (let ((internal-constraints-property
                             (getf present-properties :internal-type-variable-constraints)))
                        (if (null internal-constraints-property)
                            (make-property :internal-type-variable-constraints
                                           (copy-list internal-type-variable-constraints)
                                           :value-copy-fn
                                           (if type-settable-p
                                               #'(lambda (constr-list)
                                                   (mapcar #'copy-cps-constraint
                                                           constr-list))
                                               (constantly nil))
                                           :value-setting-fn
                                           (if type-settable-p
                                               +property/writable+
                                               +property/read-only+))
                            (when internal-constraints-supplied-p
                              (setf (property/value internal-constraints-property)
                                 (copy-list internal-type-variable-constraints))
                              nil)))))
                 (if type-settable-p
                     '(:input-type :output-type :internal-type-variable-constraints))
                 '(:input-type :output-type :internal-type-variable-constraints))))))

(defparameter *type-info-string-function-package*
  (make-info-string-function-package
   :name :type
   :info-string-fn-getter
   #'(lambda (object-class object)
       (declare (ignore object-class object))
       #'(lambda (object)
           (let ((*print-circle* nil))
             (format nil "TYPE: ~A -> ~A"
                     (type-entity/description-string (object/input-type object))
                     (type-entity/description-string (object/output-type object))))))))

#|
#'(lambda (kind)
(alexandria:switch (kind :test #'kind-equal)
  (+kind/node+
   #'(lambda (node event &rest args)
       (let ((connection (getf args :connection))
             (graph (getf args :graph)))
         (case event
           (:on-initialization
            (let ((input-type (object/input-type node))
                  (output-type (object/output-type node)))
              (dolist (constr (object/internal-type-variable-constraints
                               node))
                (cps-constraint/establish constr input-type output-type))))
           (:on-addition-to-graph
            (when (node/reference-master-source? node)
              (let ((node-input-type-property
                     (properties/get-property
                      (object/properties node) :input-type))
                    (node-output-type-property
                     (properties/get-property
                      (object/properties node) :output-type))
                    (graph-input-type-property
                     (properties/get-property
                      (object/properties graph) :input-type))
                    (graph-output-type-property
                     (properties/get-property
                      (object/properties graph) :output-type)))
                (property/register-value-setting-event-function!
                 node-input-type-property 'strong-typing nil
                 #'(lambda (value)
                     (setf (property/value graph-output-type-property)
                           value))
                 (property/value node-input-type-property))
                (property/register-value-setting-event-function!
                 node-output-type-property 'strong-typing nil
                 #'(lambda (value)
                     (setf (property/value graph-input-type-property)
                           value))
                 (property/value node-output-type-property)))))
           (:on-deletion-from-graph
            #|FIIIIIIX|# nil)
           (:on-setting-of-connection
            #|FIIIIIIX|# nil)
           (:on-loss-of-connection
            #|FIIIIIIX|# nil)))))
  (+kind/connection+
   #'(lambda (connection event &rest args)
       (case event
         (:on-addition-to-graph
          #|FIIIIIIX|# nil)
         (:on-deletion-from-graph
          #|FIIIIIIX|# nil))))
  (t (constantly nil))))
|#
