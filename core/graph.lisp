;;;; graph.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defclass object/graph (abstract-object)
  ((container :initarg :container
              :initform (cl-graph:make-graph
                         'cl-graph:graph-container
                         :default-edge-type :directed))
   (subgraph-properties-copy-fn
    :accessor graph/subgraph-properties-copy-function
    :initarg :subgraph-properties-copy-fn
    :initform (constantly nil))))

(defun object/graph? (object)
  (typep object 'object/graph))

(defun graph/regular? (graph)
  (purpose-equal (object/purpose graph) +purpose/regular+))

(define-description-string-method (object/graph 'graph)
  "")



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

(defun ~edge-container/find-connection (container connection)
  (let ((section (assoc (object/purpose connection) container
                        :test *purpose-test*)))
    (if section
        (find connection (cdr section)
              :test #'connection-equal))))

(defun ~edge-container/connection-present? (container connection)
  (not (null (~edge-container/find-connection container connection))))

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

(defmacro ~call-event-handlers (alternative-event-handler object event &rest event-handler-args)
  `(let ((args (list ,object ,event ,@event-handler-args)))
     (if ,alternative-event-handler
         (apply ,alternative-event-handler args)
         (function-collection/call-all-functions
          (object/event-handler-function-collection ,object) args))))



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

(defun graph/other-node (graph label connection)
  (graph/node graph (connection/other-node-label connection label)))

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



(defun graph/add-node! (graph node &key node-constraint-fn
                                     node-event-handler-fn graph-event-handler-fn)
  (when (and node (object/purpose node) (null (object/owner node)))
    (let ((vertex (~graph/vertex graph (node/label node))))
      (when (and (null vertex)
                 (if node-constraint-fn
                     (funcall node-constraint-fn node graph)
                     (function-collection/call-all-functions
                      (object/constraint-function-collection node)
                      (list node graph))))
        (cl-graph:add-vertex (~graph/container graph) node)
        (setf (slot-value node 'owner) graph)
        (~call-event-handlers node-event-handler-fn node :on-addition-to-graph
                              :graph graph)
        (~call-event-handlers graph-event-handler-fn graph :on-node-addition
                              :node node)
        t))))

(defun graph/add-nodes! (graph nodes &key node-constraint-fn
                                       node-event-handler-fn graph-event-handler-fn)
  (count-if #'(lambda (node)
                (graph/add-node!
                 graph node
                 :node-constraint-fn node-constraint-fn
                 :node-event-handler-fn node-event-handler-fn
                 :graph-event-handler-fn graph-event-handler-fn))
            nodes))

(flet ((signal-node-deletion-event (graph vertex connection-event-handler-fn
                                          adjacent-node-event-handler-fn
                                          graph-event-handler-fn)
         (let* ((deleted-node (cl-graph:element vertex))
                (deleted-label (node/label deleted-node)))
           (macrolet ((signal-event (edge node src-node tgt-node)
                        `(dolist (conn (~edge-container/connections
                                        (cl-graph:element ,edge)))
                           (~call-event-handlers connection-event-handler-fn
                                                 conn :on-deletion-from-graph
                                                 :source ,src-node
                                                 :target ,tgt-node
                                                 :graph graph)
                           (~call-event-handlers adjacent-node-event-handler-fn
                                                 ,node :on-loss-of-connection
                                                 :connection conn
                                                 :other-node deleted-node
                                                 :graph graph)
                           (~call-event-handlers graph-event-handler-fn
                                                 graph :on-disconnection
                                                 :connection conn
                                                 :source ,src-node
                                                 :target ,tgt-node))))
             (cl-graph:iterate-source-edges
              vertex #'(lambda (edge)
                         (let ((node (cl-graph:element (cl-graph:target-vertex edge))))
                           (unless (label-equal deleted-label (node/label node))
                             (signal-event edge node deleted-node node)))))
             (cl-graph:iterate-target-edges
              vertex #'(lambda (edge)
                         (let ((node (cl-graph:element (cl-graph:source-vertex edge))))
                           (unless (label-equal deleted-label (node/label node))
                             (signal-event edge node node deleted-node)))))
             (let ((edge (~graph/edge graph deleted-label deleted-label)))
               (if edge
                   (signal-event edge deleted-node deleted-node deleted-node)))))))

  (defun graph/delete-node! (graph label &key node-event-handler-fn
                                           connection-event-handler-fn
                                           adjacent-node-event-handler-fn
                                           graph-event-handler-fn)
    (let ((vertex (~graph/vertex graph label)))
      (when vertex
        (signal-node-deletion-event graph vertex
                                    connection-event-handler-fn
                                    adjacent-node-event-handler-fn
                                    graph-event-handler-fn)
        (~call-event-handlers node-event-handler-fn
                              (cl-graph:element vertex) :on-deletion-from-graph
                              :graph graph)
        (~call-event-handlers graph-event-handler-fn
                              graph :on-node-deletion :node (cl-graph:element vertex))
        (setf (slot-value (cl-graph:element vertex) 'owner) nil)
        (cl-graph:delete-vertex (~graph/container graph) vertex)
        t))))

(defun graph/delete-nodes! (graph labels-list &key node-event-handler-fn
                                                connection-event-handler-fn
                                                adjacent-node-event-handler-fn
                                                graph-event-handler-fn)
  (count-if #'(lambda (label)
                (graph/delete-node!
                 graph label
                 :node-event-handler-fn node-event-handler-fn
                 :connection-event-handler-fn connection-event-handler-fn
                 :adjacent-node-event-handler-fn adjacent-node-event-handler-fn
                 :graph-event-handler-fn graph-event-handler-fn))
            labels-list))



(defun graph/fitting-node? (graph node &key node-constraint-fn)
  (if node-constraint-fn
      (funcall node-constraint-fn node graph)
      (function-collection/call-all-functions
       (object/constraint-function-collection node)
       (list node graph))))

(defun graph/revise-node! (graph label &key node-constraint-fn)
  (unless (graph/fitting-node? graph (graph/node graph label)
                               :node-constraint-fn node-constraint-fn)
    (graph/delete-node! graph label)))

(defun graph/revise-nodes! (graph labels-list &key node-constraint-fn)
  (count-if #'(lambda (label)
                (graph/revise-node! graph label
                                    :node-constraint-fn node-constraint-fn))
            labels-list))

(defun graph/revise-all-nodes! (graph &key node-constraint-fn)
  (graph/revise-nodes! graph (mapcar #'node/label (graph/all-nodes graph))
                       :node-constraint-fn node-constraint-fn))



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

(defun graph/connect! (graph connection &key connection-constraint-fn
                                          node-event-handler-fn
                                          connection-event-handler-fn
                                          graph-event-handler-fn)
  (when (and connection (object/purpose connection) (null (object/owner connection)))
    (multiple-value-bind (edge src-vertex tgt-vertex)
        (~graph/edge graph (connection/source-label connection)
                     (connection/target-label connection))
      (when (and (and src-vertex tgt-vertex)
                 (if connection-constraint-fn
                     (funcall connection-constraint-fn
                              connection graph
                              (cl-graph:element src-vertex)
                              (cl-graph:element tgt-vertex))
                     (function-collection/call-all-functions
                      (object/constraint-function-collection connection)
                      (list connection graph
                            (cl-graph:element src-vertex)
                            (cl-graph:element tgt-vertex)))))
        (if (null edge)
            (cl-graph:add-edge-between-vertexes
             (~graph/container graph) src-vertex tgt-vertex
             :value (~make-connections-container connection))
            (~edge-container/add-connection! (cl-graph:element edge) connection))
        (setf (slot-value connection 'owner) graph)
        (~call-event-handlers node-event-handler-fn
                              (cl-graph:element src-vertex) :on-setting-of-connection
                              :connection connection
                              :other-node (cl-graph:element tgt-vertex)
                              :graph graph)
        (~call-event-handlers node-event-handler-fn
                              (cl-graph:element tgt-vertex) :on-setting-of-connection
                              :connection connection
                              :other-node (cl-graph:element src-vertex)
                              :graph graph)
        (~call-event-handlers connection-event-handler-fn
                              connection :on-addition-to-graph
                              :source (cl-graph:element src-vertex)
                              :target (cl-graph:element tgt-vertex)
                              :graph graph)
        (~call-event-handlers graph-event-handler-fn
                              graph :on-connection
                              :connection connection
                              :source (cl-graph:element src-vertex)
                              :target (cl-graph:element tgt-vertex))
        t))))

(defun graph/connect-set! (graph connections &key connection-constraint-fn
                                               node-event-handler-fn
                                               connection-event-handler-fn
                                               graph-event-handler-fn)
  (count-if #'(lambda (conn)
                (graph/connect!
                 graph conn
                 :connection-constraint-fn connection-constraint-fn
                 :node-event-handler-fn node-event-handler-fn
                 :connection-event-handler-fn connection-event-handler-fn
                 :graph-event-handler-fn graph-event-handler-fn))
            connections))

(defun graph/disconnect! (graph connection &key node-event-handler-fn
                                             connection-event-handler-fn
                                             graph-event-handler-fn)
  (let ((edge (~graph/edge graph (connection/source-label connection)
                           (connection/target-label connection))))
    (when edge
      (let ((src-vertex (cl-graph:source-vertex edge))
            (tgt-vertex (cl-graph:target-vertex edge))
            (connection (~edge-container/find-connection
                         (cl-graph:element edge) connection)))
        (when connection
          (~call-event-handlers connection-event-handler-fn
                                connection :on-deletion-from-graph
                                :source (cl-graph:element src-vertex)
                                :target (cl-graph:element tgt-vertex)
                                :graph graph)
          (~call-event-handlers node-event-handler-fn
                                (cl-graph:element src-vertex) :on-loss-of-connection
                                :connection connection
                                :other-node (cl-graph:element tgt-vertex)
                                :graph graph)
          (~call-event-handlers node-event-handler-fn
                                (cl-graph:element tgt-vertex) :on-loss-of-connection
                                :connection connection
                                :other-node (cl-graph:element src-vertex)
                                :graph graph)
          (~call-event-handlers graph-event-handler-fn
                                graph :on-disconnection
                                :connection connection
                                :source (cl-graph:element src-vertex)
                                :target (cl-graph:element tgt-vertex))
          (setf (slot-value connection 'owner) nil)
          (~edge-container/delete-connection! (cl-graph:element edge) connection)
          (if (~edge-container/empty? (cl-graph:element edge))
              (cl-graph:delete-edge (~graph/container graph) edge))
          t)))))

(defun graph/disconnect-set! (graph connections &key node-event-handler-fn
                                                  connection-event-handler-fn
                                                  graph-event-handler-fn)
  (count-if #'(lambda (conn)
                (graph/disconnect!
                 graph conn
                 :node-event-handler-fn node-event-handler-fn
                 :connection-event-handler-fn connection-event-handler-fn
                 :graph-event-handler-fn graph-event-handler-fn))
            connections))



(defun graph/fitting-connection? (graph connection &key connection-constraint-fn)
  (let ((source-node (graph/node graph (connection/source-label connection)))
        (target-node (graph/node graph (connection/target-label connection))))
    (if (and source-node target-node)
        (if connection-constraint-fn
            (funcall connection-constraint-fn
                     connection graph source-node target-node)
            (function-collection/call-all-functions
             (object/constraint-function-collection connection)
             (list connection graph source-node target-node))))))

(defun graph/revise-connection! (graph connection &key connection-constraint-fn)
  (unless (graph/fitting-connection?
           graph connection
           :connection-constraint-fn connection-constraint-fn)
    (graph/disconnect! graph connection)))

(defun graph/revise-connections! (graph connections &key connection-constraint-fn)
  (count-if #'(lambda (conn)
                (graph/revise-connection!
                 graph conn
                 :connection-constraint-fn connection-constraint-fn))
            connections))

(defun graph/revise-related-connections! (graph labels-list &key connection-constraint-fn)
  (graph/revise-connections!
   graph (graph/related-connections graph labels-list)
   :connection-constraint-fn connection-constraint-fn))

(defun graph/revise-all-connections! (graph &key connection-constraint-fn)
  (graph/revise-connections!
   graph (graph/all-connections graph)
   :connection-constraint-fn connection-constraint-fn))



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
  (let ((graph (make-object 'object/graph args)))
    (graph/add-nodes! graph nodes)
    (graph/connect-set! graph connections)
    graph))

(defun graph/copy-graph (graph &rest args)
  (let* ((nodes (mapcar #'copy-node (graph/all-nodes graph)))
         (connections (mapcar #'copy-connection (graph/all-connections graph)))
         (new-graph
          (copy-abstract-object
           graph (nconc (if (null (getf args :subgraph-properties-copy-fn))
                            (list :subgraph-properties-copy-fn
                                  (graph/subgraph-properties-copy-function graph)))
                        (alexandria:delete-from-plist args :subgraph-properties-copy-fn)))))
    (graph/add-nodes! new-graph nodes)
    (graph/connect-set! new-graph connections)
    new-graph))

(defmethod copy-object ((object object/graph) &rest args)
  (apply (alexandria:curry #'graph/copy-graph object) args))

(defun graph/copy-subgraph (graph labels-list &rest args)
  (let* ((nodes (mapcar #'copy-node (graph/nodes graph labels-list)))
         (existing-labels (mapcar #'node/label nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-labels)))
         (subgraph
          (copy-abstract-object
           graph (nconc
                  (if (null (getf args :subgraph-properties-copy-fn))
                      (list :subgraph-properties-copy-fn
                            (graph/subgraph-properties-copy-function graph)))
                  (if (null (getf args :properties))
                      (list :properties
                            (let ((properties-transfer-fn
                                   (getf args :properties-transfer-fn
                                         (graph/subgraph-properties-copy-function graph))))
                              (funcall properties-transfer-fn (object/properties graph)))))
                  (alexandria:delete-from-plist args :subgraph-properties-copy-fn)))))
    (graph/add-nodes! subgraph nodes)
    (graph/connect-set! subgraph connections)
    subgraph))
