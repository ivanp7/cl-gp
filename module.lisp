;;;; module.lisp

(in-package #:cl-gp)

;;; *** graph ***

(defparameter *external-world-node-id* :external-world)

(defparameter *graph/hide-world* nil)



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



(defun make-genotype-graph (&optional nodes connections)
  (let ((graph (make-graph 'graph-container :default-edge-type :directed)))
    (dolist (node nodes)
      (graph/add-node! graph node))
    (if nodes
        (dolist (conn connections)
          (graph/connect! graph conn)))
    graph))

(defun graph/all-nodes (graph &key (except-world-node *graph/hide-world*))
  (let ((nodes (mapcar #'element (vertexes graph))))
    (if (null except-world-node)
        nodes
        (delete-if #'(lambda (node)
                       (id-equal (node/id node)
                                 *external-world-node-id*))
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
        (if (null vertex)
            (add-vertex graph node)))))

(defun graph/delete-node! (graph id)
  (let ((vertex (~graph/vertex graph id)))
    (if vertex (delete-vertex graph vertex))))

(defun graph/delete-nodes! (graph ids)
  (iterate:iter (for id in ids)
                (counting (graph/delete-node! graph id))))



(macrolet ((edges->connections (edges-sexp ignore-world-var)
             (alexandria:with-gensyms (connections edge arrow id conn)
               `(let ((,connections
                       (reduce #'nconc
                               (mapcar #'(lambda (,edge)
                                           (mapcar #'(lambda (,arrow)
                                                       (make-connection
                                                        ,arrow
                                                        (element (source-vertex ,edge))
                                                        (element (target-vertex ,edge))))
                                                   (element ,edge)))
                                       ,edges-sexp)
                               :from-end t)))
                  (if (not ,ignore-world-var)
                      ,connections
                      (delete-if #'(lambda (,conn)
                                     (or (id-equal (connection/source-id ,conn)
                                                  *external-world-node-id*)
                                        (id-equal (connection/target-id ,conn)
                                                  *external-world-node-id*)))
                                 ,connections))))))

  (defun graph/input-connections (graph target-ids &key (except-world-connections
                                                         *graph/hide-world*))
    (edges->connections
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
     except-world-connections))

  (defun graph/output-connections (graph source-ids &key (except-world-connections
                                                          *graph/hide-world*))
    (edges->connections
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
     except-world-connections))

  (defun graph/external-connections (graph ids &key (except-world-connections
                                                     *graph/hide-world*))
    (nconc (graph/input-connections
            graph ids :except-world-connections except-world-connections)
           (graph/output-connections
            graph ids :except-world-connections except-world-connections)))

  (defun graph/connections (graph source-ids target-ids &key (except-world-connections
                                                              *graph/hide-world*))
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
      (if (< (length source-ids) (length target-ids))
          (conn-macro source-ids source-edges)
          (conn-macro target-ids target-edges))))

  (defun graph/internal-connections (graph ids &key (except-world-connections
                                                     *graph/hide-world*))
    (graph/connections ids ids :except-world-connections except-world-connections))

  (defun graph/all-connections (graph &key (except-world-connections *graph/hide-world*))
    (edges->connections (edges graph) except-world-connections)))



(defun graph/matching-connection-exist? (graph connection)
  (let ((edge (~graph/edge graph (connection/source-id connection)
                           (connection/target-id connection))))
    (if edge
        (not (null (member (connection/arrow connection)
                    (element edge)
                    :test #'arrow-equal))))))

(defun graph/connect! (graph connection
                       &optional (constraints-cf *constraints-conjoint-function*))
  (multiple-value-bind (edge src-vertex tgt-vertex)
      (~graph/edge graph (connection/source-id connection)
                   (connection/target-id connection))
    (if (and (and src-vertex tgt-vertex)
           (funcall constraints-cf (element src-vertex) (element tgt-vertex)
                    (connection/arrow connection) graph))
        (if (null edge)
            (add-edge-between-vertexes graph src-vertex tgt-vertex
                                       :value (list (connection/arrow connection)))
            (pushnew (connection/arrow connection) (element edge)
                     :test #'arrow-equal)))))

(defun graph/disconnect! (graph connection)
  (let ((edge (~graph/edge graph (connection/source-id connection)
                           (connection/target-id connection))))
    (when (and edge (member (connection/arrow connection)
                     (element edge)
                     :test #'arrow-equal))
      (setf (element edge)
         (delete (connection/arrow connection)
                 (element edge)
                 :test #'arrow-equal))
      (if (null (element edge))
          (delete-edge graph edge))
      t)))



(defun graph/copy-graph (graph &key (except-world-node *graph/hide-world*))
  (let* ((nodes
          (mapcar #'copy-genotype-node
                  (graph/all-nodes graph
                                   :except-world-node except-world-node)))
         (connections
          (mapcar #'copy-connection
                  (graph/all-connections graph
                                         :except-world-connections except-world-node))))
    (make-genotype-graph nodes connections)))

(defun graph/copy-subgraph (graph ids)
  (let* ((nodes (mapcar #'copy-genotype-node (graph/nodes graph ids)))
         (existing-ids (mapcar #'node/id nodes))
         (connections (mapcar #'copy-connection
                              (graph/internal-connections graph existing-ids))))
    (make-genotype-graph nodes connections)))

(defun graph/insert-subgraph! (graph subgraph)
  (let ((common-nodes-ids (mapcar #'node/id
                                  (graph/all-nodes subgraph
                                                   :except-world-node nil)))
        (common-nodes-external-conn
         (graph/external-connections graph common-nodes-ids
                                     :except-world-connections nil)))
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
  (let ((input-conn (graph/input-connections graph ids
                                             :except-world-connections nil))
        (output-conn (graph/output-connections graph ids
                                               :except-world-connections nil)))
    (graph/delete-nodes! graph ids)
    (graph/insert-subgraph! graph subgraph)
    (dolist (conn (delete nil (mapcar input-conn-fn input-conn)))
      (graph/connect! graph conn))
    (dolist (conn (delete nil (mapcar output-conn-fn output-conn)))
      (graph/connect! graph conn))
    t))

;;; *** module ***

(defclass genotype/module ()
  ((graph :reader module/graph
          :initarg :graph
          :initform (make-genotype-graph
                     (list (make-genotype-node *external-world-node-id*))))
   (properties :reader module/properties
               :initarg :properties
               :initform nil)
   (print-function :accessor module/print-function
                   :initarg :print-function
                   :initform (constantly ""))))

(defmethod print-object ((instance genotype/module) st)
  (print-unreadable-object (instance st :identity t)
    (with-slots (properties print-function) instance
      (format st "MODULE ~A" (funcall print-function properties)))))

(defun make-genotype-module (&key properties (print-function (constantly "")))
  (make-instance 'genotype/module
                 :properties properties
                 :print-function print-function))

(defun copy-genotype-module (module)
  (make-instance 'genotype/module
                 :graph (copy-graph (module/graph module))
                 :properties (funcall *properties-copy-function* (module/properties module))
                 :print-function (module/print-function module)))
