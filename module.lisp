;;;; module.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defparameter *world-node-id* :external-world)



(defun ~graph/vertex (graph id)
  (search-for-vertex graph id
                     :key #'(lambda (v)
                              (node/id (element v)))
                     :test *node/id-test*
                     :error-if-not-found? nil))

(defun ~graph/edge (graph source-id target-id)
  (let ((src-vertex (~graph/vertex graph source-id))
        (tgt-vertex (~graph/vertex graph target-id)))
    (if (and src-vertex tgt-vertex)
        (values (find-edge-between-vertexes graph src-vertex tgt-vertex
                                            :error-if-not-found? nil)
                src-vertex
                tgt-vertex))))



(defun graph/all-nodes (graph &key except-world-node)
  (let ((nodes (mapcar #'element (vertexes graph))))
    (if (null except-world-node)
        nodes
        (delete-if #'(lambda (node)
                       (id-equal (node/id node)
                                 *world-node-id*))
                   nodes))))

(defun graph/node (graph id)
  (let ((vertex (~graph/vertex graph id)))
    (if vertex (element vertex))))

(defun graph/nodes (graph ids)
  (delete nil (mapcar #'(lambda (id)
                        (graph/node graph id))
                    ids)))

(defun graph/add-node! (graph node)
  (if node
      (let ((vertex (~graph/vertex graph (node/id node))))
        (when (null vertex)
          (add-vertex graph node)
          (funcall (node/addition-to-graph-event-handler node)
                   node graph)
          t))))

(defun ~graph/signal-node-deletion-event (graph vertex)
  (let* ((deleted-node (element vertex))
         (deleted-id (node/id deleted-node)))
    (iterate-source-edges
     vertex
     #'(lambda (edge)
         (let* ((source-node (element (source-vertex edge)))
                (source-id (node/id source-node)))
           (dolist (arrow (element edge))
             (let ((conn (make-connection arrow source-id deleted-id)))
               (funcall (arrow/deletion-from-graph-event-handler arrow)
                        conn graph)
               (funcall (node/loss-of-connection-event-handler source-node)
                        source-node conn graph))))))
    (iterate-target-edges
     vertex
     #'(lambda (edge)
         (let* ((target-node (element (target-vertex edge)))
                (target-id (node/id target-node)))
           (dolist (arrow (element edge))
             (let ((conn (make-connection arrow deleted-id target-id)))
               (funcall (arrow/deletion-from-graph-event-handler arrow)
                        conn graph)
               (funcall (node/loss-of-connection-event-handler target-node)
                        target-node conn graph))))))
    (funcall (node/deletion-from-graph-event-handler deleted-node)
             deleted-node graph)))

(defun graph/delete-node! (graph id)
  (let ((vertex (~graph/vertex graph id)))
    (when vertex
      (~graph/signal-node-deletion-event graph vertex)
      (delete-vertex graph vertex)
      t)))

(defun graph/delete-nodes! (graph ids)
  (iterate:iter (for id in ids)
                (counting (graph/delete-node! graph id))))



(macrolet ((edges->connections (edges-sexp except-world-var)
             (alexandria:with-gensyms (connections edge arrow id conn)
               `(let ((,connections
                       (alexandria:mappend
                        #'(lambda (,edge)
                            (mapcar #'(lambda (,arrow)
                                        (make-connection
                                         ,arrow
                                         (element (source-vertex ,edge))
                                         (element (target-vertex ,edge))))
                                    (element ,edge)))
                        ,edges-sexp)
                        :from-end t))
                  (if (not ,except-world-var)
                      ,connections
                      (delete-if #'(lambda (,conn)
                                     (or (id-equal (connection/source-id ,conn)
                                                  *world-node-id*)
                                        (id-equal (connection/target-id ,conn)
                                                  *world-node-id*)))
                                 ,connections))))))

  (defun graph/input-connections (graph target-ids &key except-world-connections
                                                     connections-order-fn)
    (let ((connections (edges->connections
                        (iterate:iter
                          (for id in target-ids)
                          (for vertex = (~graph/vertex graph id))
                          (when vertex
                            (nconcing (delete-if
                                       #'(lambda (edge)
                                           (member (node/id (element (source-vertex edge)))
                                              target-ids
                                              :test *node/id-test*))
                                       (target-edges vertex)))))
                        except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/output-connections (graph source-ids &key except-world-connections
                                                      connections-order-fn)
    (let ((connections (edges->connections
                        (iterate:iter
                          (for id in source-ids)
                          (for vertex = (~graph/vertex graph id))
                          (when vertex
                            (nconcing (delete-if
                                       #'(lambda (edge)
                                           (member (node/id (element (target-vertex edge)))
                                              source-ids
                                              :test *node/id-test*))
                                       (source-edges vertex)))))
                        except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/external-connections (graph ids &key except-world-connections
                                                 connections-order-fn)
    (let ((connections (nconc (graph/input-connections
                               graph ids
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn)
                              (graph/output-connections
                               graph ids
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn))))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/connections (graph source-ids target-ids &key except-world-connections
                                                          connections-order-fn
                                                          collection-method)
    (macrolet ((conn-macro (ids-var edges-fn)
                 `(edges->connections
                   (iterate:iter
                     (for id in ,ids-var)
                     (for vertex = (~graph/vertex graph id))
                     (when vertex
                       (nconcing
                        (delete-if-not
                         #'(lambda (edge)
                             (and (member (node/id (element (source-vertex edge)))
                                   source-ids
                                   :test *node/id-test*)
                                (member (node/id (element (target-vertex edge)))
                                   target-ids
                                   :test *node/id-test*)))
                         (,edges-fn vertex)))))
                   except-world-connections)))
      (let ((connections
             (case collection-method
               (:source (conn-macro source-ids source-edges))
               (:target (conn-macro target-ids target-edges))
               (t (alexandria:whichever
                   (conn-macro source-ids source-edges)
                   (conn-macro target-ids target-edges))))))
        (if (null connections-order-fn)
            connections
            (sort connections connections-order-fn)))))

  (defun graph/internal-connections (graph ids &key except-world-connections
                                                 connections-order-fn)
    (graph/connections ids ids
                       :except-world-connections except-world-connections
                       :connections-order-fn connections-order-fn))

  (defun graph/related-connections (graph ids &key except-world-connections
                                                connections-order-fn)
    (let ((connections (nconc (graph/internal-connections
                               graph ids
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn)
                              (graph/external-connections
                               graph ids
                               :except-world-connections except-world-connections
                               :connections-order-fn connections-order-fn))))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn))))

  (defun graph/all-connections (graph &key except-world-connections
                                        connections-order-fn)
    (let ((connections (edges->connections (edges graph) except-world-connections)))
      (if (null connections-order-fn)
          connections
          (sort connections connections-order-fn)))))



(defun graph/matching-connection-exist? (graph connection)
  (let ((edge (~graph/edge graph (connection/source-id connection)
                           (connection/target-id connection))))
    (if edge
        (not (null (member (connection/arrow connection)
                    (element edge)
                    :test #'arrow-equal))))))

(defun graph/connect! (graph connection
                       &optional (constraint-fn *constraints-conjoint-function*))
  (multiple-value-bind (edge src-vertex tgt-vertex)
      (~graph/edge graph (connection/source-id connection)
                   (connection/target-id connection))
    (when (and (and src-vertex tgt-vertex)
             (funcall constraint-fn (element src-vertex) (element tgt-vertex)
                      (connection/arrow connection) graph))
      (let ((arrow (connection/arrow connection)))
        (if (null edge)
            (add-edge-between-vertexes graph src-vertex tgt-vertex
                                       :value (list arrow))
            (pushnew arrow (element edge) :test #'arrow-equal))
        (funcall (arrow/addition-to-graph-event-handler arrow)
                 connection graph)
        (funcall (node/setting-of-connection-event-handler (element src-vertex))
                 (element src-vertex) connection graph)
        (funcall (node/setting-of-connection-event-handler (element tgt-vertex))
                 (element tgt-vertex) connection graph)
        t))))

(defun graph/disconnect! (graph connection)
  (let ((edge (~graph/edge graph (connection/source-id connection)
                           (connection/target-id connection))))
    (when (and edge (member (connection/arrow connection)
                     (element edge)
                     :test #'arrow-equal))
      (let ((src-vertex (source-vertex edge))
            (tgt-vertex (target-vertex edge)))
        (funcall (arrow/deletion-from-graph-event-handler arrow)
                 connection graph)
        (funcall (node/loss-of-connection-event-handler (element src-vertex))
                 (element src-vertex) connection graph)
        (funcall (node/loss-of-connection-event-handler (element tgt-vertex))
                 (element tgt-vertex) connection graph)
        (setf (element edge)
           (delete (connection/arrow connection)
                   (element edge)
                   :test #'arrow-equal))
        (if (null (element edge))
            (delete-edge graph edge))
        t))))



(defun graph/make-graph (&optional nodes connections)
  (let ((graph (make-graph 'graph-container :default-edge-type :directed)))
    (dolist (node nodes)
      (graph/add-node! graph node))
    (if nodes
        (dolist (conn connections)
          (graph/connect! graph conn)))
    graph))

(defun graph/copy-graph (graph &key except-world-node)
  (let* ((nodes
          (mapcar #'copy-node
                  (graph/all-nodes graph
                                   :except-world-node except-world-node)))
         (connections
          (mapcar #'copy-connection
                  (graph/all-connections graph
                                         :except-world-connections except-world-node))))
    (graph/make-graph nodes connections)))

(defun graph/copy-subgraph (graph ids)
  (let* ((nodes (mapcar #'copy-genotype-node (graph/nodes graph ids)))
         (existing-ids (mapcar #'node/id nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-ids))))
    (graph/make-graph nodes connections)))

(defun graph/insert-subgraph! (graph subgraph)
  (let ((common-nodes-ids (mapcar #'node/id
                                  (graph/all-nodes subgraph)))
        (common-nodes-external-conn
         (graph/external-connections graph common-nodes-ids)))
    (graph/delete-nodes! graph common-nodes-ids)
    (dolist (node (graph/all-nodes subgraph))
      (graph/add-node! graph node))
    (dolist (conn (graph/all-connections subgraph))
      (graph/connect! graph conn))
    (dolist (conn common-nodes-external-conn)
      (graph/connect! graph conn))
    t))

(defun graph/replace-nodes! (graph ids subgraph &key (input-conn-fn (constantly nil))
                                                  (output-conn-fn (constantly nil)))
  (let ((input-conn (graph/input-connections graph ids))
        (output-conn (graph/output-connections graph ids)))
    (graph/delete-nodes! graph ids)
    (graph/insert-subgraph! graph subgraph)
    (dolist (conn (delete nil (mapcar input-conn-fn input-conn)))
      (graph/connect! graph conn))
    (dolist (conn (delete nil (mapcar output-conn-fn output-conn)))
      (graph/connect! graph conn))
    t))

;;; *** module ***

(defparameter *module/print-functions-list* nil)
(defparameter *world-node/print-functions-list* nil)

(defclass object/module ()
  ((graph :reader module/graph
          :initarg :graph
          :initform (error "OBJECT/MODULE -- :graph parameter must be supplied"))
   (properties :reader module/properties
               :initarg :properties
               :initform nil)
   (print-function :accessor module/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance object/module) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (properties print-function) instance
      (format st "MODULE ~A" (funcall print-function properties)))))

(defun make-module (&key properties (print-function (make-conjoint-print-function
                                                     *module/print-functions-list*))
                      world-node-properties
                      (world-node-print-function (make-conjoint-print-function
                                                  *world-node/print-functions-list*)))
  (make-instance 'object/module
                 :graph (graph/make-graph
                         (list (make-node *world-node-id*
                                          :properties world-node-properties
                                          :print-function world-node-print-function)))
                 :properties properties
                 :print-function print-function))

(defun copy-module (module)
  (make-instance 'object/module
                 :graph (copy-graph (module/graph module))
                 :properties (funcall *properties-copy-function* (module/properties module))
                 :print-function (module/print-function module)))

(defun module/ensure-world-node-existence! (module &key world-node-properties
                                                     (addition-to-graph-fn (constantly nil))
                                                     (deletion-from-graph-fn (constantly nil))
                                                     (setting-of-connection-fn (constantly nil))
                                                     (loss-of-connection-fn (constantly nil))
                                                     (world-node-print-function
                                                      (make-conjoint-print-function
                                                       *world-node/print-functions-list*)))
  (if (null (graph/node (module/graph module) *world-node-id*))
      (graph/add-node! (module/graph module)
                       (make-node *world-node-id*
                                  :properties world-node-properties
                                  :addition-to-graph-fn addition-to-graph-fn
                                  :deletion-from-graph-fn deletion-from-graph-fn
                                  :setting-of-connection-fn setting-of-connection-fn
                                  :loss-of-connection-fn loss-of-connection-fn
                                  :print-function world-node-print-function))))
