;;;; programs.lisp

(in-package #:cl-gp)

;;;; В этом файле должны быть все ограничения, интерпретатор и прочие функции

;;; *** selector ***

(defparameter *tag-test* #'eql)

(defclass data-field-selector (abstract-selector)
  ((tags :accessor selector/tags
         :initarg :tags
         :initform nil)))

(defmethod selector/description-string ((selector data-field-selector))
  (let ((*print-circle* nil))
    (format nil "~:S" (selector/tags selector))))

(defun make-data-field-selector (tags-list)
  (make-instance 'data-field-selector :tags (copy-list tags-list)))

(defmethod copy-selector ((selector data-field-selector))
  (make-data-field-selector (selector/tags selector)))

(defmethod selector-equal ((selector1 data-field-selector) (selector2 data-field-selector))
  (let ((tags1 (selector/tags selector1))
        (tags2 (selector/tags selector2)))
    (and (= (length tags1) (length tags2))
       (every *tag-test* tags1 tags2))))

;;; *** object name ***

(defun object/name (object)
  (properties/get-property-value (object/properties object) :name))

(defparameter *name-functionality*
  (make-structural-constraint
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
                        (push (make-fn-alist-entry 'name-functionality
                                                   (cons node nil)
                                                   #'(lambda (value)
                                                       (setf (property/value graph-name-property)
                                                          value)))
                              (property/on-value-setting-event-functions-alist
                               node-name-property))
                        (setf (property/value node-name-property)
                           (property/value node-name-property)))))
                   (:on-deletion-from-graph
                    (let* ((node-name-property (properties/get-property
                                                (object/properties node) :name))
                           (name (property/value node-name-property)))
                      (setf (property/value node-name-property) nil)
                      (setf (property/on-value-setting-event-functions-alist
                          node-name-property)
                         (delete-if #'(lambda (entry)
                                        (and (eql (fn-alist-entry/setter entry)
                                                'name-functionality)
                                           (eql (car (fn-alist-entry/unique-id entry))
                                                node)))
                                    (property/on-value-setting-event-functions-alist
                                     node-name-property)))
                      (setf (property/value node-name-property) name)))
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
                        (push (make-fn-alist-entry 'name-functionality
                                                   (cons node other-node)
                                                   #'(lambda (value)
                                                       (setf (property/value
                                                           other-node-name-property)
                                                          value)))
                              (property/on-value-setting-event-functions-alist
                               node-name-property))
                        (setf (property/value node-name-property)
                           (property/value node-name-property)))))
                   (:on-loss-of-connection
                    (when (and (node/reference-source? node)
                             (connection/reference? connection))
                      (let* ((other-node (graph/node graph
                                                     (connection/other-node-label
                                                      connection (node/label node))))
                             (node-name-property (properties/get-property
                                                  (object/properties node) :name)))
                        (setf (property/on-value-setting-event-functions-alist
                            node-name-property)
                           (delete-if #'(lambda (entry)
                                          (and (eql (fn-alist-entry/setter entry)
                                                  'name-functionality)
                                             (eql (car (fn-alist-entry/unique-id entry))
                                                  node)
                                             (eql (cdr (fn-alist-entry/unique-id entry))
                                                  other-node)))
                                      (property/on-value-setting-event-functions-alist
                                       node-name-property)
                                      :count 1))
                        (object/set-property-value! other-node :name nil)))))))))
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
  #|TODO|#
  (declare (ignore graph))
  nil)



#|
(defparameter *feedforward-constraint-function* (constantly t)) ; dummy
(defparameter *finite-recursion-constraint-function* (constantly t)) ; dummy
|#
