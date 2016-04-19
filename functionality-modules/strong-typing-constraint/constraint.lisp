;;;; constraint.lisp

(in-package #:cl-gp)

;;; *** connection CPS constraint ***

(defclass cps-constraint/connection (cps/abstract-constraint)
  ((source-type-entity :initarg :source)
   (target-type-entity :initarg :target)))

(defmethod cps-constraint/establish ((this cps-constraint/connection))
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
  (let ((input-type (getf args :input-type))
        (output-type (getf args :output-type)))
    (setf type-variables-list (funcall type-variables-list-getter input-type output-type)))
  (dolist (typevar type-variables-list)
    (cps-connector/connect! (slot-value typevar 'cps-connector) this)))

(defmethod cps-constraint/abolish ((this cps-constraint/node))
  (dolist (typevar type-variables-list)
    (cps-connector/disconnect! (slot-value typevar 'cps-connector) this)))

(defmethod initialize-instance :after ((this cps-constraint/node))
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



(defun make-node-type-variable-constraint (type-variables-list-getter
                                           &optional cps-constraint-fn)
  (make-instance 'cps-constraint/node
                 :type-variables-list-getter type-variables-list-getter
                 :cps-constraint-fn cps-constraint-fn))

(defmethod copy-cps-constraint ((cps-constraint cps-constraint/node))
  (with-slots (type-variables-list-getter cps-constraint-fn) cps-constraint
    (make-node-type-variable-constraint type-variables-list-getter
                                        cps-constraint-fn)))

;;; *** generic ***

(defun object/input-type (object)
  (object/get-property-value object :input-type +bottom-type+))

(defun object/output-type (object)
  (object/get-property-value object :output-type +bottom-type+))

(defun object/type (object direction)
  (cond
    ((direction/input? direction) (object/input-type object))
    ((direction/output? direction) (object/output-type object))
    (t (error "OBJECT/TYPE -- incorrect direction ~S is supplied" direction))))

(defun object/internal-type-variable-constraints-list (object)
  (object/get-property-value object :internal-type-variable-constraints-list))

;;; ******************
;;; *** constraint ***
;;; ******************

(defparameter *strong-typing-enabled* t)

(defparameter *strong-typing-constraint*
  (make-functionality-module
   :name 'strong-typing
   :constraint-connection-test-fn
   #'(lambda (source-node target-node connection graph)
       (declare (ignore graph))
       (or (not *strong-typing-enabled*)
          (let ((source-entity (type/nested-subtype-selection
                                (object/output-type source-node)
                                (arrow/source-selector (connection/arrow connection))))
                (target-entity (type/nested-subtype-selection
                                (object/input-type target-node)
                                (arrow/target-selector (connection/arrow connection)))))
            (type-entity/reducible? source-entity target-entity))))
   :event-handler-fn-getter
   #'(lambda (kind)
       (alexandria:switch (kind :test #'kind-equal)
         (+kind/node+ #'(lambda (node event &rest args)
                          (case event
                            (:on-initialization
                             (let ((input-type (object/input-type node))
                                   (output-type (object/output-type node)))
                               (dolist (constr (object/internal-type-variable-constraints-list
                                                node))
                                 (cps-constraint/establish constr input-type output-type))))
                            (...))))
         (+kind/connection+ #'(lambda (connection event &rest args) ...))
         (+kind/graph+ #'(lambda (graph event &rest args) ...))
         (t (constantly nil))))
   :init-args-getter
   #'(lambda (kind)
       (if (kind-equal kind +kind/node+)
           '(:input-type :output-type :internal-type-variable-constraints-list)))
   :properties-constr-fn-getter
   #'(lambda (kind)
       (alexandria:switch (kind :test #'kind-equal)
         (+kind/node+
          #'(lambda (&key (input-type +bottom-type+) (output-type +bottom-type+)
                  internal-type-variable-constraints-list)
              (make-properties-container
               (list (make-property :input-type input-type
                                    :value-copy-fn #'copy-type-entity)
                     (make-property :output-type output-type
                                    :value-copy-fn #'copy-type-entity)
                     (make-property :internal-type-variable-constraints-list
                                    internal-type-variable-constraints-list
                                    :value-copy-fn #'(lambda (constraints)
                                                       (mapcar #'copy-cps-constraint
                                                               constraints)))))))
         (+kind/connection+
          #'(lambda ()
              (make-properties-container
               (list (make-property :input-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :output-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :internal-type-variable-constraints-list
                                    nil
                                    :value-copy-fn (constantly nil))))))
         (+kind/graph+
          #'(lambda ()
              (make-properties-container
               (list (make-property :input-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))
                     (make-property :output-type +bottom-type+
                                    :value-copy-fn (constantly +bottom-type+))))))
         (t (constantly nil))))))

(defparameter *type-info-string-function-package*
  (make-functionality-info-string-function-package
   :name :type
   :info-string-fn-getter
   #'(lambda (kind)
       (declare (ignore kind))
       #'(lambda (object)
           (let ((*print-circle* nil))
             (format nil "{TYPE ~S -> ~S}"
                     (object/input-type object)
                     (object/output-type object)))))))
