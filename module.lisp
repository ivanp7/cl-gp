;;;; module.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defparameter *world-node-label* t)

(defun world-node-label? (label)
  (label-equal label *world-node-label*))



(defun ~graph/vertex (graph label)
  (cl-graph:search-for-vertex graph label
                              :key #'(lambda (v)
                                       (node/label (cl-graph:element v)))
                              :test *node/label-test*
                              :error-if-not-found? nil))

(defun ~graph/edge (graph source-label target-label)
  (let ((src-vertex (~graph/vertex graph source-label))
        (tgt-vertex (~graph/vertex graph target-label)))
    (if (and src-vertex tgt-vertex)
        (values (cl-graph:find-edge-between-vertexes
                 graph src-vertex tgt-vertex :error-if-not-found? nil)
                src-vertex
                tgt-vertex))))



(defun graph/all-nodes (graph &key except-world-node)
  (let ((nodes (mapcar #'cl-graph:element (cl-graph:vertexes graph))))
    (if (null except-world-node)
        nodes
        (delete-if #'(lambda (node)
                       (world-node-label? (node/label node)))
                   nodes))))

(defun graph/node (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex (cl-graph:element vertex))))

(defun graph/nodes (graph labels-list)
  (delete nil (mapcar #'(lambda (label)
                        (graph/node graph label))
                    labels-list)))

(defun graph/node-input-neighbours (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex (mapcar #'cl-graph:element
                       (cl-graph:parent-vertexes vertex)))))

(defun graph/node-output-neighbours (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex (mapcar #'cl-graph:element
                       (cl-graph:child-vertexes vertex)))))

(defun graph/node-neighbours (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex (mapcar #'cl-graph:element
                       (cl-graph:neighbor-vertexes vertex)))))



(defun graph/add-node! (graph node)
  (if node
      (let ((vertex (~graph/vertex graph (node/label node))))
        (when (null vertex)
          (cl-graph:add-vertex graph node)
          (funcall (node/events-handler-function node)
                   node :on-addition-to-graph
                   :graph graph)
          t))))

(defun graph/add-nodes! (graph nodes)
  (iterate:iter (for node in nodes)
                (counting (graph/add-node! graph node))))

(defun ~graph/signal-node-deletion-event (graph vertex)
  (let* ((deleted-node (cl-graph:element vertex))
         (deleted-label (node/label deleted-node)))
    (cl-graph:iterate-source-edges
     vertex
     #'(lambda (edge)
         (let* ((source-node (cl-graph:element (cl-graph:source-vertex edge)))
                (source-label (node/label source-node)))
           (dolist (arrow (cl-graph:element edge))
             (let ((conn (make-connection arrow source-label deleted-label)))
               (funcall (arrow/events-handler-function arrow)
                        conn :on-deletion-from-graph
                        :connection conn :graph graph)
               (funcall (node/events-handler-function source-node)
                        source-node :on-loss-of-connection
                        :connection conn :graph graph))))))
    (cl-graph:iterate-target-edges
     vertex
     #'(lambda (edge)
         (let* ((target-node (cl-graph:element (cl-graph:target-vertex edge)))
                (target-label (node/label target-node)))
           (dolist (arrow (cl-graph:element edge))
             (let ((conn (make-connection arrow deleted-label target-label)))
               (funcall (arrow/events-handler-function arrow)
                        conn :on-deletion-from-graph
                        :connection conn :graph graph)
               (funcall (node/events-handler-function target-node)
                        target-node :on-loss-of-connection
                        :connection conn :graph graph))))))
    (funcall (node/events-handler-function deleted-node)
             deleted-node :on-deletion-from-graph
             :graph graph)))

(defun graph/delete-node! (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (when vertex
      (~graph/signal-node-deletion-event graph vertex)
      (cl-graph:delete-vertex graph vertex)
      t)))

(defun graph/delete-nodes! (graph labels-list)
  (iterate:iter (for label in labels-list)
                (counting (graph/delete-node! graph label))))



(macrolet ((edges->connections (edges-sexp except-world-var)
             (alexandria:with-gensyms (connections edge arrow conn)
               `(let ((,connections
                       (alexandria:mappend
                        #'(lambda (,edge)
                            (mapcar #'(lambda (,arrow)
                                        (make-connection
                                         ,arrow
                                         (node/label (cl-graph:element
                                                      (cl-graph:source-vertex ,edge)))
                                         (node/label (cl-graph:element
                                                      (cl-graph:target-vertex ,edge)))))
                                    (cl-graph:element ,edge)))
                        ,edges-sexp)))
                  (if (not ,except-world-var)
                      ,connections
                      (delete-if #'(lambda (,conn)
                                     (or (world-node-label? (connection/source-label ,conn))
                                        (world-node-label? (connection/target-label ,conn))))
                                 ,connections))))))

  (defun graph/input-connections (graph target-labels &key except-world-connections
                                                        connections-order-fn)
    (let ((connections (edges->connections
                        (iterate:iter
                          (for label in target-labels)
                          (for vertex = (~graph/vertex graph label))
                          (when vertex
                            (nconcing (delete-if
                                       #'(lambda (edge)
                                           (member (node/label (cl-graph:element
                                                           (cl-graph:source-vertex edge)))
                                              target-labels
                                              :test *node/label-test*))
                                       (cl-graph:target-edges vertex)))))
                        except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/output-connections (graph source-labels &key except-world-connections
                                                         connections-order-fn)
    (let ((connections (edges->connections
                        (iterate:iter
                          (for label in source-labels)
                          (for vertex = (~graph/vertex graph label))
                          (when vertex
                            (nconcing (delete-if
                                       #'(lambda (edge)
                                           (member (node/label (cl-graph:element
                                                           (cl-graph:target-vertex edge)))
                                              source-labels
                                              :test *node/label-test*))
                                       (cl-graph:source-edges vertex)))))
                        except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/external-connections (graph labels-list &key except-world-connections
                                                         connections-order-fn)
    (let ((connections (nconc (graph/input-connections
                               graph labels-list
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn)
                              (graph/output-connections
                               graph labels-list
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn))))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/connections (graph source-labels target-labels &key except-world-connections
                                                                connections-order-fn
                                                                collection-method)
    (macrolet ((conn-macro (labels-var edges-fn)
                 `(edges->connections
                   (iterate:iter
                     (for label in ,labels-var)
                     (for vertex = (~graph/vertex graph label))
                     (when vertex
                       (nconcing
                        (delete-if-not
                         #'(lambda (edge)
                             (and (member (node/label (cl-graph:element (cl-graph:source-vertex edge)))
                                   source-labels
                                   :test *node/label-test*)
                                (member (node/label (cl-graph:element (cl-graph:target-vertex edge)))
                                   target-labels
                                   :test *node/label-test*)))
                         (,edges-fn vertex)))))
                   except-world-connections)))
      (let ((connections
             (case collection-method
               (:source (conn-macro source-labels cl-graph:source-edges))
               (:target (conn-macro target-labels cl-graph:target-edges))
               (t (alexandria:whichever
                   (conn-macro source-labels cl-graph:source-edges)
                   (conn-macro target-labels cl-graph:target-edges))))))
        (if (null connections-order-fn)
            connections
            (sort connections connections-order-fn)))))

  (defun graph/internal-connections (graph labels-list &key except-world-connections
                                                         connections-order-fn)
    (graph/connections graph labels-list labels-list
                       :except-world-connections except-world-connections
                       :connections-order-fn connections-order-fn))

  (defun graph/related-connections (graph labels-list &key except-world-connections
                                                        connections-order-fn)
    (let ((connections (nconc (graph/internal-connections
                               graph labels-list
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn)
                              (graph/external-connections
                               graph labels-list
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn))))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/all-connections (graph &key except-world-connections
                                        connections-order-fn)
    (let ((connections (edges->connections (cl-graph:edges graph)
                                          except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn)))))



(defun graph/matching-connection-exist? (graph connection)
  (let ((edge (~graph/edge graph (connection/source-label connection)
                           (connection/target-label connection))))
    (if edge
        (not (null (member (connection/arrow connection)
                    (cl-graph:element edge)
                    :test #'arrow-equal))))))

(defun graph/connect! (graph connection &key (constraint-fn
                                              *constraints-conjoint-function*))
  (multiple-value-bind (edge src-vertex tgt-vertex)
      (~graph/edge graph (connection/source-label connection)
                   (connection/target-label connection))
    (when (and (and src-vertex tgt-vertex)
             (funcall constraint-fn (cl-graph:element src-vertex) (cl-graph:element tgt-vertex)
                      (connection/arrow connection) graph))
      (let ((arrow (connection/arrow connection)))
        (if (null edge)
            (cl-graph:add-edge-between-vertexes
             graph src-vertex tgt-vertex :value (list arrow))
            (pushnew arrow (cl-graph:element edge) :test #'arrow-equal))
        (funcall (arrow/events-handler-function arrow)
                 connection :on-addition-to-graph
                 :graph graph)
        (funcall (node/events-handler-function (cl-graph:element src-vertex))
                 (cl-graph:element src-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        (funcall (node/events-handler-function (cl-graph:element tgt-vertex))
                 (cl-graph:element tgt-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        t))))

(defun graph/connect-set! (graph connections &key (constraint-fn
                                                   *constraints-conjoint-function*))
  (iterate:iter (for conn in connections)
                (counting (graph/connect! graph conn
                                          :constraint-fn constraint-fn))))

(defun graph/disconnect! (graph connection)
  (let ((edge (~graph/edge graph (connection/source-label connection)
                           (connection/target-label connection))))
    (when (and edge (member (connection/arrow connection)
                     (cl-graph:element edge)
                     :test #'arrow-equal))
      (let ((src-vertex (cl-graph:source-vertex edge))
            (tgt-vertex (cl-graph:target-vertex edge)))
        (funcall (arrow/events-handler-function (connection/arrow connection))
                 connection :on-deletion-from-graph
                 :graph graph)
        (funcall (node/events-handler-function (cl-graph:element src-vertex))
                 (cl-graph:element src-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (funcall (node/events-handler-function (cl-graph:element tgt-vertex))
                 (cl-graph:element tgt-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (setf (cl-graph:element edge)
           (delete (connection/arrow connection)
                   (cl-graph:element edge)
                   :test #'arrow-equal))
        (if (null (cl-graph:element edge))
            (cl-graph:delete-edge graph edge))
        t))))

(defun graph/disconnect-set! (graph connections)
  (iterate:iter (for conn in connections)
                (counting (graph/disconnect! graph conn))))



(defun graph/fitting-connection? (graph connection &key (constraint-fn
                                                         *constraints-conjoint-function*))
  (let ((source-node (graph/node graph (connection/source-label connection)))
        (target-node (graph/node graph (connection/target-label connection))))
    (if (and source-node target-node)
        (funcall constraint-fn
                 source-node target-node
                 (connection/arrow connection) graph))))

(defun graph/revise-connection! (graph connection &key (constraint-fn
                                                        *constraints-conjoint-function*))
  (unless (graph/fitting-connection? graph connection :constraint-fn constraint-fn)
    (graph/disconnect! graph connection)))

(defun graph/revise-connections! (graph connections &key (constraint-fn
                                                          *constraints-conjoint-function*))
  (iterate:iter (for conn in connections)
                (counting (graph/revise-connection!
                           graph conn :constraint-fn constraint-fn))))

(defun graph/revise-related-connections! (graph labels-list &key (constraint-fn
                                                                  *constraints-conjoint-function*))
  (graph/revise-connections! graph (graph/related-connections graph labels-list)
                             :constraint-fn constraint-fn))

(defun graph/revise-all-connections! (graph &key (constraint-fn
                                                  *constraints-conjoint-function*))
  (graph/revise-connections! graph (graph/all-connections graph)
                             :constraint-fn constraint-fn))



(defun graph/insert-subgraph! (graph subgraph)
  (let* ((common-nodes-labels (mapcar #'node/label
                                      (graph/all-nodes subgraph)))
         (external-common-nodes-conn
          (graph/external-connections graph common-nodes-labels)))
    (graph/delete-nodes! graph common-nodes-labels)
    (graph/add-nodes! graph (graph/all-nodes subgraph))
    (graph/connect-set! graph (graph/all-connections subgraph))
    (graph/connect-set! graph external-common-nodes-conn)
    t))

(defun graph/replace-nodes! (graph labels-list subgraph &key (input-conn-fn #'identity)
                                                          (output-conn-fn #'identity))
  (let ((input-connections (graph/input-connections graph labels-list))
        (output-connections (graph/output-connections graph labels-list)))
    (graph/delete-nodes! graph labels-list)
    (graph/insert-subgraph! graph subgraph)
    (graph/connect-set! graph (delete nil (mapcar input-conn-fn input-connections)))
    (graph/connect-set! graph (delete nil (mapcar output-conn-fn output-connections)))
    t))



(defun graph/make-graph (&optional nodes connections)
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed)))
    (graph/add-nodes! graph nodes)
    (graph/connect-set! graph connections)
    graph))

(defun graph/copy-graph (graph &key except-world-node)
  (let* ((nodes (mapcar #'copy-node
                        (graph/all-nodes
                         graph :except-world-node except-world-node)))
         (connections (mapcar #'copy-connection
                              (graph/all-connections
                               graph :except-world-connections except-world-node))))
    (graph/make-graph nodes connections)))

(defun graph/copy-subgraph (graph labels-list)
  (let* ((nodes (mapcar #'copy-node (graph/nodes graph labels-list)))
         (existing-labels (mapcar #'node/label nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-labels))))
    (graph/make-graph nodes connections)))

;;; *** module ***

(defparameter *module/print-functions-list* nil)
(defparameter *world-node/print-functions-list* nil)

(defclass object/module ()
  ((graph :reader module/graph
          :initarg :graph
          :initform (error "MODULE -- :graph parameter must be supplied"))
   (associated-nodes :reader module/associated-nodes
                     :initform (make-hash-table))
   (print-function :accessor module/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/module) st)
  (print-unreadable-object (instance st)
    (with-slots (print-function) instance
      (let ((info (funcall print-function
                           (module/world-node-properties instance))))
        (format st (concatenate 'string
                                "MODULE"
                                (if (plusp (length info)) " " "")
                                info))))))

(defun make-module (&key wn-properties
                      (wn-events-handler-fn (constantly nil))
                      (wn-print-function (make-conjoint-print-function
                                          *world-node/print-functions-list*))
                      (module-print-function (make-conjoint-print-function
                                              *module/print-functions-list*)))
  (make-instance 'object/module
                 :graph (graph/make-graph
                         (list (make-node *world-node-label*
                                          :properties wn-properties
                                          :events-handler-fn wn-events-handler-fn
                                          :print-function wn-print-function)))
                 :print-function module-print-function))

(defun copy-module (module)
  (make-instance 'object/module
                 :graph (graph/copy-graph (module/graph module))
                 :print-function (module/print-function module)))

(defun module/world-node (module)
  (graph/node (module/graph module) *world-node-label*))

(defun module/world-node-properties (module)
  (let ((world-node (graph/node (module/graph module) *world-node-label*)))
    (if world-node
        (node/properties world-node))))



(defun associate-node-with-module (node module)
  (when (node/primitive? node)
    (setf (slot-value node 'module) module)
    (setf (gethash node (slot-value module 'associated-nodes)) t)
    (funcall (node/events-handler-function node) node :on-association)
    (graph/revise-related-connections! (module/graph module)
                                       (list (node/label node)))
    t))

(defun unassociate-node (node)
  (when (node/module-associated? node)
    (with-slots (module) node
      (let ((module-object module))
        (remhash node (slot-value module 'associated-nodes))
        (setf module nil)
        (funcall (node/events-handler-function node) node :on-unassociation)
        (graph/revise-related-connections! (module/graph module-object)
                                           (list (node/label node))))
      t)))

(defun copy-module-node (node)
  (let ((new-node (copy-primitive-node node)))
    (associate-node-with-module new-node (node/associated-module node))
    new-node))
