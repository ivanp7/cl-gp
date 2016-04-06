;;;; graph.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defclass object/graph ()
  ((container :initarg :container
              :initform (error "GRAPH -- :container parameter must be supplied"))
   (properties :reader graph/properties
               :initarg :properties
               :initform nil)
   (info-string-fn :accessor graph/info-string-function
                   :initarg :info-string-fn
                   :initform (constantly ""))))

(defun graph/description-string (graph &key no-object-class)
  (let ((descr (let ((*print-circle* nil))
                 (with-slots (info-string-fn) graph
                   (let ((info (funcall info-string-fn graph)))
                     (concatenate 'string
                                  (if (and (not no-object-class)
                                         (plusp (length info)))
                                      " " "")
                                  info))))))
    (if no-object-class
        descr
        (concatenate 'string "GRAPH" descr))))

(defmethod print-object ((instance object/graph) st)
  (print-unreadable-object (instance st)
    (format st (graph/description-string instance))))



(defun ~graph/vertex (graph label)
  (cl-graph:search-for-vertex (slot-value graph 'container) label
                              :key #'(lambda (v)
                                       (node/label (cl-graph:element v)))
                              :test *label-test*
                              :error-if-not-found? nil))

(defun ~graph/edge (graph source-label target-label)
  (let ((src-vertex (~graph/vertex graph source-label))
        (tgt-vertex (~graph/vertex graph target-label)))
    (if (and src-vertex tgt-vertex)
        (values (cl-graph:find-edge-between-vertexes
                 (slot-value graph 'container)
                 src-vertex tgt-vertex :error-if-not-found? nil)
                src-vertex
                tgt-vertex))))



(defun ~make-connections-container (&optional connection)
  (if connection
      (acons (connection/purpose connection) (list connection) nil)
      nil))

(defun ~edge-container/empty? (container)
  (null container))

(defun ~edge-container/purpose-present? (container purpose)
  (not (null (assoc purpose container :test *purpose-test*))))

(defun ~edge-container/connection-present? (container connection)
  (let ((section (assoc (connection/purpose connection) container
                        :test *purpose-test*)))
    (if section
        (not (null (find connection (cdr section)
                       :test #'connection-equal))))))

(defun ~edge-container/add-connection-fn (container connection)
  (let ((section (assoc (connection/purpose connection) container
                        :test *purpose-test*)))
    (if section
        (progn (push connection (cdr section)) container)
        (acons (connection/purpose connection) (list connection) container))))

(defmacro ~edge-container/add-connection! (container connection)
  `(setf ,container (~edge-container/add-connection-fn ,container ,connection)))

(defun ~edge-container/delete-connection-fn (container connection)
  (let* ((purpose (connection/purpose connection))
         (section (assoc purpose container :test *purpose-test*)))
    (if section
        (progn
          (setf (cdr section)
             (delete connection (cdr section)
                     :test #'connection-equal :count 1))
          (if (null (cdr section))
              (delete-if #'(lambda (sect)
                             (purpose-equal (car sect) purpose))
                         container :count 1)
              container))
        container)))

(defmacro ~edge-container/delete-connection! (container connection)
  `(setf ,container (~edge-container/delete-connection-fn ,container ,connection)))

(defun ~edge-container/purposes (container)
  (mapcar #'car container))

(defun ~edge-container/connections (container &optional purpose)
  (if purpose
      (copy-list (cdr (assoc purpose container :test *purpose-test*)))
      (alexandria:mappend #'(lambda (section)
                              (copy-list (cdr section)))
                          container)))



(defun graph/all-nodes (graph &key purpose)
  (if (null purpose)
      (mapcar #'cl-graph:element
              (cl-graph:vertexes (slot-value graph 'container)))
      (mapcar #'cl-graph:element
              (cl-graph:find-vertexes-if
               (slot-value graph 'container)
               #'(lambda (vertex)
                   (purpose-equal purpose
                                  (node/purpose (cl-graph:element vertex))))))))

(defun graph/nodes-of-group (graph group-label &key purpose)
  (mapcar #'cl-graph:element
          (cl-graph:find-vertexes-if
           (slot-value graph 'container)
           #'(lambda (vertex)
               (and (or (null purpose)
                     (purpose-equal purpose
                                    (node/purpose (cl-graph:element vertex))))
                  (member group-label (node/groups (cl-graph:element vertex))
                     :test *label-test*))))))

(defun graph/node (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex
        (cl-graph:element vertex))))

(defun graph/nodes (graph labels-list)
  (delete nil (mapcar #'(lambda (label)
                        (graph/node graph label))
                    labels-list)))

(macrolet ((define-neighbours-function (name neighbour-vertexes-expr
                                             edges-expr
                                             adjacent-vertex-expr)
             `(defun ,name (graph label &key purpose connections-purpose)
                (let ((vertex (~graph/vertex graph label)))
                  (if vertex
                      (let ((nodes (if (null connections-purpose)
                                       (mapcar #'cl-graph:element
                                               ,neighbour-vertexes-expr)
                                       (mapcar #'(lambda (edge)
                                                   (cl-graph:element ,adjacent-vertex-expr))
                                               (delete-if-not
                                                #'(lambda (edge)
                                                    (~edge-container/purpose-present?
                                                     (cl-graph:element edge)
                                                     connections-purpose))
                                                ,edges-expr)))))
                        (if (null purpose)
                            nodes
                            (delete-if-not
                             #'(lambda (node)
                                 (purpose-equal purpose (node/purpose node)))
                             nodes))))))))

  (define-neighbours-function graph/node-input-neighbours
      (cl-graph:parent-vertexes vertex)
    (cl-graph:target-edges vertex)
    (cl-graph:source-vertex edge))

  (define-neighbours-function graph/node-output-neighbours
      (cl-graph:child-vertexes vertex)
    (cl-graph:source-edges vertex)
    (cl-graph:target-vertex edge))

  (define-neighbours-function graph/node-neighbours
      (cl-graph:neighbor-vertexes vertex)
    (cl-graph:edges vertex)
    (cl-graph:other-vertex edge vertex)))



(defun graph/add-node! (graph node &key node-event-handler-fn)
  (if node
      (let ((vertex (~graph/vertex graph (node/label node))))
        (when (null vertex)
          (cl-graph:add-vertex (slot-value graph 'container) node)
          (funcall (if node-event-handler-fn
                       node-event-handler-fn
                       (node/events-handler-function node))
                   node :on-addition-to-graph
                   :graph graph)
          t))))

(defun graph/add-nodes! (graph nodes &key node-event-handler-fn)
  (iterate:iter (for node in nodes)
                (counting (graph/add-node!
                           graph node
                           :node-event-handler-fn node-event-handler-fn))))

(defun ~graph/signal-node-deletion-event (graph vertex node-event-handler-fn
                                          connection-event-handler-fn
                                          adjacent-node-event-handler-fn)
  (let* ((deleted-node (cl-graph:element vertex))
         (deleted-label (node/label deleted-node)))
    (macrolet ((signal-event-fn (vertex-fn)
                 `#'(lambda (edge)
                      (let ((node (cl-graph:element (,vertex-fn edge))))
                        (unless (label-equal deleted-label (node/label node))
                          (dolist (conn (~edge-container/connections
                                         (cl-graph:element edge)))
                            (funcall (if connection-event-handler-fn
                                         connection-event-handler-fn
                                         (connection/events-handler-function conn))
                                     conn :on-deletion-from-graph :graph graph)
                            (funcall (if adjacent-node-event-handler-fn
                                         adjacent-node-event-handler-fn
                                         (node/events-handler-function node))
                                     node :on-loss-of-connection
                                     :connection conn :graph graph)))))))
      (cl-graph:iterate-source-edges
       vertex (signal-event-fn cl-graph:source-vertex))
      (cl-graph:iterate-target-edges
       vertex (signal-event-fn cl-graph:target-vertex)))
    (let ((edge (~graph/edge graph deleted-label deleted-label)))
      (if edge
          (dolist (conn (~edge-container/connections
                         (cl-graph:element edge)))
            (funcall (if connection-event-handler-fn
                         connection-event-handler-fn
                         (connection/events-handler-function conn))
                     conn :on-deletion-from-graph :graph graph)
            (funcall (if node-event-handler-fn
                         node-event-handler-fn
                         (node/events-handler-function deleted-node))
                     deleted-node :on-loss-of-connection
                     :connection conn :graph graph))))
    (funcall (if node-event-handler-fn
                 node-event-handler-fn
                 (node/events-handler-function deleted-node))
             deleted-node :on-deletion-from-graph
             :graph graph)))

(defun graph/delete-node! (graph label &key node-event-handler-fn
                                         connection-event-handler-fn
                                         adjacent-node-event-handler-fn)
  (let ((vertex (~graph/vertex graph label)))
    (when vertex
      (~graph/signal-node-deletion-event graph vertex
                                         node-event-handler-fn
                                         connection-event-handler-fn
                                         adjacent-node-event-handler-fn)
      (cl-graph:delete-vertex (slot-value graph 'container) vertex)
      t)))

(defun graph/delete-nodes! (graph labels-list &key node-event-handler-fn
                                                connection-event-handler-fn
                                                adjacent-node-event-handler-fn)
  (iterate:iter (for label in labels-list)
                (counting (graph/delete-node!
                           graph label
                           :node-event-handler-fn node-event-handler-fn
                           :connection-event-handler-fn connection-event-handler-fn
                           :adjacent-node-event-handler-fn adjacent-node-event-handler-fn))))



(flet ((edges->connections (purpose edges)
         (alexandria:mappend #'(lambda (edge)
                                 (~edge-container/connections
                                  (cl-graph:element edge) purpose))
                             edges))
       (edges->purposes (purpose edges)
         (if purpose
             (list purpose)
             (reduce #'(lambda (purposes1 purposes2)
                         (nunion purposes1 purposes2 :test *purpose-test*))
                     (mapcar #'(lambda (edge)
                                 (~edge-container/purposes
                                  (cl-graph:element edge)))
                             edges)
                     :from-end t
                     :initial-value nil))))

  (defun graph/input-connections (graph target-labels &key purpose connections-order-fn)
    (let* ((edges (iterate:iter
                   (for label in target-labels)
                   (for vertex = (~graph/vertex graph label))
                   (when vertex
                     (nconcing (delete-if
                                #'(lambda (edge)
                                    (member (node/label (cl-graph:element
                                                    (cl-graph:source-vertex edge)))
                                       target-labels
                                       :test *label-test*))
                                (cl-graph:target-edges vertex))))))
           (connections (edges->connections purpose edges))
           (purposes (edges->purposes purpose edges)))
      (values (if (null connections-order-fn)
                  connections
                  (sort connections connections-order-fn))
              purposes)))

  (defun graph/output-connections (graph source-labels &key purpose connections-order-fn)
    (let* ((edges (iterate:iter
                   (for label in source-labels)
                   (for vertex = (~graph/vertex graph label))
                   (when vertex
                     (nconcing (delete-if
                                #'(lambda (edge)
                                    (member (node/label (cl-graph:element
                                                    (cl-graph:target-vertex edge)))
                                       source-labels
                                       :test *label-test*))
                                (cl-graph:source-edges vertex))))))
           (connections (edges->connections purpose edges))
           (purposes (edges->purposes purpose edges)))
      (values (if (null connections-order-fn)
                  connections
                  (sort connections connections-order-fn))
              purposes)))

  (defun graph/external-connections (graph labels-list &key purpose connections-order-fn)
    (multiple-value-bind (input-conn input-purposes)
        (graph/input-connections graph labels-list
                                 :purpose purpose
                                 :connections-order-fn connections-order-fn)
      (multiple-value-bind (output-conn output-purposes)
          (graph/output-connections graph labels-list
                                    :purpose purpose
                                    :connections-order-fn connections-order-fn)
        (let ((connections (nconc input-conn output-conn))
              (purposes (nunion input-purposes output-purposes
                                :test *purpose-test*)))
          (values (if (null connections-order-fn)
                      connections
                      (sort connections connections-order-fn))
                  purposes)))))

  (defun graph/connections (graph source-labels target-labels &key purpose
                                                                connections-order-fn
                                                                collection-method)
    (macrolet ((edges-macro (labels-var edges-fn)
                 `(iterate:iter
                   (for label in ,labels-var)
                   (for vertex = (~graph/vertex graph label))
                   (when vertex
                     (nconcing
                      (delete-if-not
                       #'(lambda (edge)
                           (and (member (node/label (cl-graph:element (cl-graph:source-vertex edge)))
                                 source-labels
                                 :test *label-test*)
                              (member (node/label (cl-graph:element (cl-graph:target-vertex edge)))
                                 target-labels
                                 :test *label-test*)))
                       (,edges-fn vertex)))))))
      (let* ((edges (case collection-method
                      (:source (edges-macro source-labels cl-graph:source-edges))
                      (:target (edges-macro target-labels cl-graph:target-edges))
                      (t (alexandria:whichever
                          (edges-macro source-labels cl-graph:source-edges)
                          (edges-macro target-labels cl-graph:target-edges)))))
             (connections (edges->connections purpose edges))
             (purposes (edges->purposes purpose edges)))
        (values (if (null connections-order-fn)
                    connections
                    (sort connections connections-order-fn))
                purposes))))

  (defun graph/internal-connections (graph labels-list &key purpose connections-order-fn)
    (graph/connections graph labels-list labels-list
                       :purpose purpose
                       :connections-order-fn connections-order-fn))

  (defun graph/related-connections (graph labels-list &key purpose connections-order-fn)
    (multiple-value-bind (internal-conn internal-purposes)
        (graph/internal-connections graph labels-list
                                    :purpose purpose
                                    :connections-order-fn connections-order-fn)
      (multiple-value-bind (external-conn external-purposes)
          (graph/external-connections graph labels-list
                                      :purpose purpose
                                      :connections-order-fn connections-order-fn)
        (let ((connections (nconc internal-conn external-conn))
              (purposes (nunion internal-purposes external-purposes
                                :test *purpose-test*)))
          (values (if (null connections-order-fn)
                      connections
                      (sort connections connections-order-fn))
                  purposes)))))

  (defun graph/all-connections (graph &key purpose connections-order-fn)
    (let* ((edges (cl-graph:edges (slot-value graph 'container)))
           (connections (edges->connections purpose edges))
           (purposes (edges->purposes purpose edges)))
      (values (if (null connections-order-fn)
                  connections
                  (sort connections connections-order-fn))
              purposes))))



(defun graph/matching-connection-exist? (graph connection)
  (let ((edge (~graph/edge graph (connection/source-label connection)
                           (connection/target-label connection))))
    (~edge-container/connection-present? (cl-graph:element edge) connection)))

(defun graph/connect! (graph connection &key (constraint-fn
                                              *constraints-conjoint-function*)
                                          node-event-handler-fn
                                          connection-event-handler-fn)
  (when (connection/purpose connection)
    (multiple-value-bind (edge src-vertex tgt-vertex)
        (~graph/edge graph (connection/source-label connection)
                     (connection/target-label connection))
      (when (and (and src-vertex tgt-vertex)
               (funcall constraint-fn
                        (cl-graph:element src-vertex)
                        (cl-graph:element tgt-vertex)
                        connection graph))
        (if (null edge)
            (cl-graph:add-edge-between-vertexes
             graph src-vertex tgt-vertex
             :value (~make-connections-container connection))
            (~edge-container/add-connection! (cl-graph:element edge) connection))
        (funcall (if connection-event-handler-fn
                     connection-event-handler-fn
                     (connection/events-handler-function connection))
                 connection :on-addition-to-graph
                 :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (node/events-handler-function (cl-graph:element src-vertex)))
                 (cl-graph:element src-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (node/events-handler-function (cl-graph:element tgt-vertex)))
                 (cl-graph:element tgt-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        t))))

(defun graph/connect-set! (graph connections &key (constraint-fn
                                                   *constraints-conjoint-function*)
                                               node-event-handler-fn
                                               connection-event-handler-fn)
  (iterate:iter (for conn in connections)
                (counting (graph/connect!
                           graph conn
                           :constraint-fn constraint-fn
                           :node-event-handler-fn node-event-handler-fn
                           :connection-event-handler-fn connection-event-handler-fn))))

(defun graph/disconnect! (graph connection &key node-event-handler-fn
                                             connection-event-handler-fn)
  (let ((edge (~graph/edge graph (connection/source-label connection)
                           (connection/target-label connection))))
    (when (and edge (~edge-container/connection-present?
                   (cl-graph:element edge) connection))
      (let ((src-vertex (cl-graph:source-vertex edge))
            (tgt-vertex (cl-graph:target-vertex edge)))
        (funcall (if connection-event-handler-fn
                     connection-event-handler-fn
                     (connection/events-handler-function connection))
                 connection :on-deletion-from-graph
                 :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (node/events-handler-function (cl-graph:element src-vertex)))
                 (cl-graph:element src-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (node/events-handler-function (cl-graph:element tgt-vertex)))
                 (cl-graph:element tgt-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (~edge-container/delete-connection! (cl-graph:element edge) connection)
        (if (~edge-container/empty? (cl-graph:element edge))
            (cl-graph:delete-edge graph edge))
        t))))

(defun graph/disconnect-set! (graph connections &key node-event-handler-fn
                                                  connection-event-handler-fn)
  (iterate:iter (for conn in connections)
                (counting (graph/disconnect!
                           graph conn
                           :node-event-handler-fn node-event-handler-fn
                           :connection-event-handler-fn connection-event-handler-fn))))



(defun graph/fitting-connection? (graph connection &key (constraint-fn
                                                         *constraints-conjoint-function*))
  (let ((source-node (graph/node graph (connection/source-label connection)))
        (target-node (graph/node graph (connection/target-label connection))))
    (if (and source-node target-node)
        (funcall constraint-fn source-node target-node connection graph))))

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



(defun graph/make-graph (nodes connections &rest args)
  (let ((graph (apply (alexandria:curry
                       #'make-instance 'object/graph
                       :container (cl-graph:make-graph
                                   'cl-graph:graph-container
                                   :default-edge-type :directed))
                      args)))
    (graph/add-nodes! graph nodes)
    (graph/connect-set! graph connections)
    graph))

(defun graph/copy-graph (graph)
  (let* ((nodes (mapcar #'copy-node (graph/all-nodes graph)))
         (connections (mapcar #'copy-connection (graph/all-connections graph))))
    (graph/make-graph nodes connections
                      :properties (copy-properties (graph/properties graph))
                      :info-string-fn (graph/info-string-function graph))))

(defun graph/copy-subgraph (graph labels-list &key (properties-transfer-fn (constantly nil))
                                                (new-info-string-fn
                                                 (graph/info-string-function graph)))
  (let* ((nodes (mapcar #'copy-node (graph/nodes graph labels-list)))
         (existing-labels (mapcar #'node/label nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-labels))))
    (graph/make-graph nodes connections
                      :properties (funcall properties-transfer-fn (graph/properties graph))
                      :info-string-fn new-info-string-fn)))
