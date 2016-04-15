;;;; graph.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defclass object/graph (abstract-object)
  ((container :initarg :container
              :initform (cl-graph:make-graph
                         'cl-graph:graph-container
                         :default-edge-type :directed))
   (constraint-test-fn :accessor graph/constraint-test-function
                       :initarg :constraint-test-fn
                       :initform nil)))

(defconstant +kind/graph+ 'graph)

(defmethod object/kind ((object object/graph))
  +kind/graph+)

(defun object/graph? (object)
  (kind-equal (object/kind object) +kind/graph+))

(defun graph/regular? (graph)
  (purpose-equal (object/purpose graph) +purpose/regular+))

(define-description-string-method object/graph
  (let ((*print-circle* nil))
    (with-slots (info-string-fn) object
      (funcall info-string-fn object))))



(defmacro ~graph/container (graph)
  `(slot-value ,graph 'container))

(defun ~graph/vertex (graph label)
  (cl-graph:search-for-vertex (~graph/container graph) label
                              :key #'(lambda (v)
                                       (node/label (cl-graph:element v)))
                              :test *label-test*
                              :error-if-not-found? nil))

(defun ~graph/edge (graph source-label target-label)
  (let ((src-vertex (~graph/vertex graph source-label))
        (tgt-vertex (~graph/vertex graph target-label)))
    (if (and src-vertex tgt-vertex)
        (values (cl-graph:find-edge-between-vertexes
                 (~graph/container graph)
                 src-vertex tgt-vertex :error-if-not-found? nil)
                src-vertex
                tgt-vertex))))



(defun ~make-connections-container (&optional connection)
  (if connection
      (acons (object/purpose connection) (list connection) nil)
      nil))

(defun ~edge-container/empty? (container)
  (null container))

(defun ~edge-container/purpose-present? (container purpose)
  (not (null (assoc purpose container :test *purpose-test*))))

(defun ~edge-container/connection-present? (container connection)
  (let ((section (assoc (object/purpose connection) container
                        :test *purpose-test*)))
    (if section
        (not (null (find connection (cdr section)
                       :test #'connection-equal))))))

(defun ~edge-container/add-connection-fn (container connection)
  (let ((section (assoc (object/purpose connection) container
                        :test *purpose-test*)))
    (if section
        (progn (push connection (cdr section)) container)
        (acons (object/purpose connection) (list connection) container))))

(defmacro ~edge-container/add-connection! (container connection)
  `(setf ,container (~edge-container/add-connection-fn ,container ,connection)))

(defun ~edge-container/delete-connection-fn (container connection)
  (let* ((purpose (object/purpose connection))
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
      (let* ((nodes (mapcar #'cl-graph:element
                            (cl-graph:vertexes (~graph/container graph)))))
        (values nodes
                (remove-duplicates (mapcar #'object/purpose nodes)
                                   :test *purpose-test*)))
      (values (mapcar #'cl-graph:element
                      (cl-graph:find-vertexes-if
                       (~graph/container graph)
                       #'(lambda (vertex)
                           (purpose-equal purpose
                                          (object/purpose (cl-graph:element vertex))))))
              (list purpose))))

(defun graph/nodes-of-group (graph group-label &key purpose)
  (let ((nodes (mapcar #'cl-graph:element
                       (cl-graph:find-vertexes-if
                        (~graph/container graph)
                        #'(lambda (vertex)
                            (and (or (null purpose)
                                  (purpose-equal purpose
                                                 (object/purpose (cl-graph:element vertex))))
                               (member group-label (node/groups (cl-graph:element vertex))
                                  :test *label-test*)))))))
    (values nodes
            (if purpose
                (list purpose)
                (remove-duplicates (mapcar #'object/purpose nodes)
                                   :test *purpose-test*)))))

(defun graph/node (graph label)
  (let ((vertex (~graph/vertex graph label)))
    (if vertex
        (cl-graph:element vertex))))

(defun graph/nodes (graph labels-list)
  (let ((nodes (delete nil (mapcar #'(lambda (label)
                                     (graph/node graph label))
                                 labels-list))))
    (values nodes
            (remove-duplicates (mapcar #'object/purpose nodes)
                               :test *purpose-test*))))

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
                                 (purpose-equal purpose (object/purpose node)))
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
          (cl-graph:add-vertex (~graph/container graph) node)
          (funcall (if node-event-handler-fn
                       node-event-handler-fn
                       (object/event-handler-function node))
                   node :on-addition-to-graph
                   :graph graph)
          (funcall (object/event-handler-function graph)
                   graph :on-node-addition
                   :node node)
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
    (macrolet ((signal-event-fn (vertex-fn src-node tgt-node)
                 `#'(lambda (edge)
                      (let ((node (cl-graph:element (,vertex-fn edge))))
                        (unless (label-equal deleted-label (node/label node))
                          (dolist (conn (~edge-container/connections
                                         (cl-graph:element edge)))
                            (funcall (if connection-event-handler-fn
                                         connection-event-handler-fn
                                         (object/event-handler-function conn))
                                     conn :on-deletion-from-graph
                                     :source ,src-node
                                     :target ,tgt-node
                                     :graph graph)
                            (funcall (if adjacent-node-event-handler-fn
                                         adjacent-node-event-handler-fn
                                         (object/event-handler-function node))
                                     node :on-loss-of-connection
                                     :connection conn :graph graph)
                            (funcall (object/event-handler-function graph)
                                     graph :on-disconnection
                                     :connection conn
                                     :source ,src-node
                                     :target ,tgt-node)))))))
      (cl-graph:iterate-source-edges
       vertex (signal-event-fn cl-graph:target-vertex deleted-node node))
      (cl-graph:iterate-target-edges
       vertex (signal-event-fn cl-graph:source-vertex node deleted-node)))
    (let ((edge (~graph/edge graph deleted-label deleted-label)))
      (if edge
          (dolist (conn (~edge-container/connections
                         (cl-graph:element edge)))
            (funcall (if connection-event-handler-fn
                         connection-event-handler-fn
                         (object/event-handler-function conn))
                     conn :on-deletion-from-graph
                     :source deleted-node
                     :target deleted-node
                     :graph graph)
            (funcall (if node-event-handler-fn
                         node-event-handler-fn
                         (object/event-handler-function deleted-node))
                     deleted-node :on-loss-of-connection
                     :connection conn :graph graph)
            (funcall (object/event-handler-function graph)
                     graph :on-disconnection
                     :connection conn
                     :source deleted-node
                     :target deleted-node))))))

(defun graph/delete-node! (graph label &key node-event-handler-fn
                                         connection-event-handler-fn
                                         adjacent-node-event-handler-fn)
  (let ((vertex (~graph/vertex graph label)))
    (when vertex
      (~graph/signal-node-deletion-event graph vertex
                                         node-event-handler-fn
                                         connection-event-handler-fn
                                         adjacent-node-event-handler-fn)
      (funcall (if node-event-handler-fn
                   node-event-handler-fn
                   (object/event-handler-function (cl-graph:element vertex)))
               (cl-graph:element vertex) :on-deletion-from-graph
               :graph graph)
      (funcall (object/event-handler-function graph)
               graph :on-node-deletion
               :node (cl-graph:element vertex))
      (cl-graph:delete-vertex (~graph/container graph) vertex)
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
    (let* ((edges (cl-graph:edges (~graph/container graph)))
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

(defun graph/connect! (graph connection &key (constraint-test-fn
                                              (graph/constraint-test-function graph))
                                          node-event-handler-fn
                                          connection-event-handler-fn)
  (when (object/purpose connection)
    (multiple-value-bind (edge src-vertex tgt-vertex)
        (~graph/edge graph (connection/source-label connection)
                     (connection/target-label connection))
      (when (and (and src-vertex tgt-vertex)
               (funcall constraint-test-fn
                        (cl-graph:element src-vertex)
                        (cl-graph:element tgt-vertex)
                        connection graph))
        (if (null edge)
            (cl-graph:add-edge-between-vertexes
             (~graph/container graph) src-vertex tgt-vertex
             :value (~make-connections-container connection))
            (~edge-container/add-connection! (cl-graph:element edge) connection))
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (object/event-handler-function (cl-graph:element src-vertex)))
                 (cl-graph:element src-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (object/event-handler-function (cl-graph:element tgt-vertex)))
                 (cl-graph:element tgt-vertex) :on-setting-of-connection
                 :connection connection :graph graph)
        (funcall (if connection-event-handler-fn
                     connection-event-handler-fn
                     (object/event-handler-function connection))
                 connection :on-addition-to-graph
                 :source (cl-graph:element src-vertex)
                 :target (cl-graph:element tgt-vertex)
                 :graph graph)
        (funcall (object/event-handler-function graph)
                 graph :on-connection
                 :connection connection
                 :source (cl-graph:element src-vertex)
                 :target (cl-graph:element tgt-vertex))
        t))))

(defun graph/connect-set! (graph connections &key (constraint-test-fn
                                                   (graph/constraint-test-function graph))
                                               node-event-handler-fn
                                               connection-event-handler-fn)
  (iterate:iter (for conn in connections)
                (counting (graph/connect!
                           graph conn
                           :constraint-test-fn constraint-test-fn
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
                     (object/event-handler-function connection))
                 connection :on-deletion-from-graph
                 :source (cl-graph:element src-vertex)
                 :target (cl-graph:element tgt-vertex)
                 :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (object/event-handler-function (cl-graph:element src-vertex)))
                 (cl-graph:element src-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (funcall (if node-event-handler-fn
                     node-event-handler-fn
                     (object/event-handler-function (cl-graph:element tgt-vertex)))
                 (cl-graph:element tgt-vertex) :on-loss-of-connection
                 :connection connection :graph graph)
        (funcall (object/event-handler-function graph)
                 graph :on-disconnection
                 :connection connection
                 :source (cl-graph:element src-vertex)
                 :target (cl-graph:element tgt-vertex))
        (~edge-container/delete-connection! (cl-graph:element edge) connection)
        (if (~edge-container/empty? (cl-graph:element edge))
            (cl-graph:delete-edge (~graph/container graph) edge))
        t))))

(defun graph/disconnect-set! (graph connections &key node-event-handler-fn
                                                  connection-event-handler-fn)
  (iterate:iter (for conn in connections)
                (counting (graph/disconnect!
                           graph conn
                           :node-event-handler-fn node-event-handler-fn
                           :connection-event-handler-fn connection-event-handler-fn))))



(defun graph/fitting-connection? (graph connection
                                  &key (constraint-test-fn
                                        (graph/constraint-test-function graph)))
  (let ((source-node (graph/node graph (connection/source-label connection)))
        (target-node (graph/node graph (connection/target-label connection))))
    (if (and source-node target-node)
        (funcall constraint-test-fn source-node target-node connection graph))))

(defun graph/revise-connection! (graph connection
                                 &key (constraint-test-fn
                                       (graph/constraint-test-function graph)))
  (unless (graph/fitting-connection? graph connection :constraint-test-fn constraint-test-fn)
    (graph/disconnect! graph connection)))

(defun graph/revise-connections! (graph connections
                                  &key (constraint-test-fn
                                        (graph/constraint-test-function graph)))
  (iterate:iter (for conn in connections)
                (counting (graph/revise-connection!
                           graph conn :constraint-test-fn constraint-test-fn))))

(defun graph/revise-related-connections! (graph labels-list
                                          &key (constraint-test-fn
                                                (graph/constraint-test-function graph)))
  (graph/revise-connections! graph (graph/related-connections graph labels-list)
                             :constraint-test-fn constraint-test-fn))

(defun graph/revise-all-connections! (graph &key (constraint-test-fn
                                                  (graph/constraint-test-function graph)))
  (graph/revise-connections! graph (graph/all-connections graph)
                             :constraint-test-fn constraint-test-fn))



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



(defmacro ~make-empty-graph (args)
  `(let ((constraint-test-fn
          (make-conjoint-constraint-test-function
           (cons (getf ,args :constraint-test-fn)
                 (mapcar #'structural-constraint/test-function
                         (getf ,args :structural-constraints
                               *structural-constraints*))))))
     (~object-init-args-handling-let (+kind/graph+ ,args)
       (make-object 'object/graph
                    (nconc (list :constraint-test-fn constraint-test-fn
                                 :properties properties-container
                                 :event-handler-fn event-handler-function
                                 :info-string-fn info-string-function)
                           (alexandria:delete-from-plist
                            ,args :constraint-test-fn
                            :properties :event-handler-fn :info-string-fn))))))

(defun graph/make-graph (nodes connections &rest args)
  (let ((graph (~make-mpty-graph args)))
    (graph/add-nodes! graph nodes)
    (graph/connect-set! graph connections)
    graph))

(defun graph/copy-graph (graph &rest args)
  (let* ((nodes (mapcar #'copy-node (graph/all-nodes graph)))
         (connections (mapcar #'copy-connection (graph/all-connections graph)))
         (new-graph (copy-object graph
                                 (nconc (list :constraint-test-fn
                                              (graph/constraint-test-function graph))
                                        args))))
    (graph/add-nodes! new-graph nodes)
    (graph/connect-set! new-graph connections)
    new-graph))

(defun graph/copy-subgraph (graph labels-list &rest args)
  (let* ((nodes (mapcar #'copy-node (graph/nodes graph labels-list)))
         (existing-labels (mapcar #'node/label nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-labels)))
         (subgraph
          (copy-object
           graph (nconc (list :constraint-test-fn
                              (graph/constraint-test-function graph)
                              :properties
                              (let ((properties-transfer-fn
                                     (getf args :properties-transfer-fn (constantly nil))))
                                (funcall properties-transfer-fn (object/properties graph))))
                        (alexandria:remove-from-plist
                         args :properties :properties-transfer-fn)))))
    (graph/add-nodes! subgraph nodes)
    (graph/connect-set! subgraph connections)
    subgraph))
