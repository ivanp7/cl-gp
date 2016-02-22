;;;; module.lisp

;;; Public functions/constants/variables of the module:
#|
node-socket/new
node-socket/node-index
node-socket/field-selector
node-socket/copy

connection/new
connection/source-socket
connection/destination-socket
connection/copy

module/connection-priority
module/connection-properties

module/new

module/name
module/properties
module/all-nodes
module/input-type
module/output-type

module/add-node!
module/delete-node!
module/node-exists?

module/node-input-connections
module/node-output-connections

module/socket-type
module/connection-possible?
module/connected?
module/connect!
module/disconnect!

module/node

module/diagram
|#

(in-package #:cl-gp)

;;; *** node metric ***

(defun metric~/new (neutral? &optional (value (if neutral? 1 0)))
  (cons neutral? value))

(defun metric~/neutral? (metric)
  (car metric))
(defun (setf metric~/neutral?) (new-value metric)
  (setf (car metric) new-value))

(defun metric~/value (metric)
  (cdr metric))
(defun (setf metric~/value) (new-value metric)
  (setf (cdr metric) new-value))

(defun metric~/1+ (metric)
  (metric~/new (metric~/neutral? metric)
               (1+ (metric~/value metric))))

(defun metric~/test= (m1 m2)
  (and (eql (metric~/neutral? m1)
          (metric~/neutral? m2))
     (= (metric~/value m1) (metric~/value m2))))

(defun metric~/test< (m1 m2)
  (cond
    ((eql (metric~/neutral? m1)
          (metric~/neutral? m2))
     (< (metric~/value m1) (metric~/value m2)))
    ((metric~/neutral? m1) nil)
    (t t)))

(defun metric~/min (m1 m2)
  (if (metric~/test< m1 m2) m1 m2))

(defun metric~/copy (metric)
  (metric~/new (metric~/neutral? metric)
               (metric~/value metric)))

;;; *** node state ***

(defun node+state~/new (node metric)
  (list node metric))

(defun node+state~/node (nstate)
  (first nstate))
(defun (setf node+state~/node) (new-value nstate)
  (setf (first nstate) new-value))

(defun node+state~/metric (nstate)
  (second nstate))
(defun (setf node+state~/metric) (new-value nstate)
  (setf (second nstate) new-value))

;;; *** node socket ***

(defun node-socket/new (index &optional field-selector)
  (cons index (copy-list field-selector)))

(defun node-socket/node-index (socket)
  (car socket))
(defun (setf node-socket/node-index) (new-value socket)
  (setf (car socket) new-value))

(defun node-socket/field-selector (socket)
  (cdr socket))
(defun (setf node-socket/field-selector) (new-value socket)
  (setf (cdr socket) new-value))

(defun node-socket/copy (socket)
  (node-socket/new (node-socket/node-index socket)
                   (copy-list (node-socket/field-selector socket))))

;;; *** connection ***

(defun connection/new (source-socket destination-socket)
  (cons source-socket destination-socket))

(defun connection/source-socket (conn)
  (car conn))
(defun (setf connection/source-socket) (new-value conn)
  (setf (car conn) new-value))

(defun connection/destination-socket (conn)
  (cdr conn))
(defun (setf connection/destination-socket) (new-value conn)
  (setf (cdr conn) new-value))

(defun connection/copy (conn)
  (connection/new (node-socket/copy (connection/source-socket conn))
                  (node-socket/copy (connection/destination-socket conn))))

;;; *** connection state ***

(defun conn-state~/new (priority &optional properties)
  (list priority properties))

(defun conn-state~/priority (conn-state)
  (first conn-state))
(defun (setf conn-state~/priority) (new-value conn-state)
  (setf (first conn-state) new-value))

(defun conn-state~/properties (conn-state)
  (second conn-state))
(defun (setf conn-state~/properties) (new-value conn-state)
  (setf (second conn-state) new-value))



(defun module~/conn-state (module conn)
  (quiver/arrow-value
   module
   (node-socket/node-index (connection/source-socket conn))
   (node-socket/node-index (connection/destination-socket conn))
   conn))

(defun module/connection-priority (module conn)
  (conn-state~/priority (module~/conn-state module conn)))
(defun (setf module/connection-priority) (new-value module conn)
  (setf (conn-state~/priority (module~/conn-state module conn)) new-value))

(defun module/connection-properties (module conn)
  (conn-state~/properties (module~/conn-state module conn)))
(defun (setf module/connection-properties) (new-value module conn)
  (setf (conn-state~/properties (module~/conn-state module conn)) new-value))

;;; *** module ***

(defun module/new (name &optional properties)
  (quiver/make-empty-quiver
   :properties (list 0 name properties)))

(defun module~/max-index (module)
  (first (quiver/properties module)))
(defun (setf module~/max-index) (new-value module)
  (setf (first (quiver/properties module)) new-value))

(defun module/name (module)
  (second (quiver/properties module)))
(defun (setf module/name) (new-value module)
  (setf (second (quiver/properties module)) new-value))

(defun module/properties (module)
  (third (quiver/properties module)))
(defun (setf module/properties) (new-value module)
  (setf (third (quiver/properties module)) new-value))

(defun module~/node+state (module index)
  (quiver/vertex-value module index))

(defun module/all-nodes (module)
  (quiver/all-vertices module))



(defun module/input-type (module)
  (record/new
   (mapcar #'(lambda (node)
               (field/new (node/name node) (node/output-type node)))
           (delete-if-not #'node/input?
                          (mapcar #'(lambda (index)
                                      (node+state~/node
                                       (module~/node+state module index)))
                                  (module/all-nodes module))))))

(defun module/output-type (module)
  (record/new
   (mapcar #'(lambda (node)
               (field/new (node/name node) (node/input-type node)))
           (delete-if-not #'node/output?
                          (mapcar #'(lambda (index)
                                      (node+state~/node
                                       (module~/node+state module index)))
                                  (module/all-nodes module))))))



(defun module/add-node! (module node)
  (let ((index (incf (module~/max-index module))))
    (quiver/add-vertex! module index
                        (node+state~/new node
                                         (metric~/new (not (node/output? node)))))
    index))

(defun module/delete-node! (module index)
  (quiver/delete-vertex! module index))

(defun module/node-exists? (module index)
  (quiver/vertex-exists? module index))



(defun module/node-input-connections (module index)
  (sort (mapcan #'(lambda (group)
                    (mapcar #'connection/copy (getf group :arrows)))
                (append (quiver/vertex-loops module index)
                        (quiver/vertex-inputs module index)))
        #'(lambda (conn1 conn2)
            (> (module/connection-priority module conn1)
               (module/connection-priority module conn2)))))

(defun module/node-output-connections (module index)
  (mapcan #'(lambda (group)
              (mapcar #'connection/copy (getf group :arrows)))
          (append (quiver/vertex-loops module index)
                  (quiver/vertex-outputs module index))))

(defun module~/update-metric (module index)
  (let ((dest-metrics
         (mapcar #'(lambda (conn)
                     (node+state~/metric
                      (module~/node+state
                       module (node-socket/node-index
                               (connection/destination-socket conn)))))
                 (module/node-output-connections module index)))
        (node-state (module~/node+state module index)))
    (if dest-metrics
        (let ((new-metric
               (metric~/1+
                (reduce #'metric~/min dest-metrics))))
          (unless (metric~/test= new-metric
                                 (node+state~/metric node-state))
            (setf (node+state~/metric node-state) new-metric)
            (dolist (conn (module/node-input-connections module index))
              (module~/update-metric module
                                     (node-socket/node-index
                                      (connection/source-socket conn))))))
        (setf (node+state~/metric node-state)
           (metric~/new (not (node/output? (node+state~/node node-state))))))))



(defun module/socket-type (module socket direction)
  (let ((node (node+state~/node
               (module~/node+state
                module (node-socket/node-index socket)))))
    (if node
        (record/nested-search
         (case direction
           (:input (node/input-type node))
           (:output (node/output-type node))
           (t (error "MODULE/SOCKET-TYPE -- incorrect socket direction ~S" direction)))
         (node-socket/field-selector socket))
        +type/bottom+)))

(defun module/connection-possible? (module conn)
  (type/compatible?
   (module/socket-type module
                       (connection/source-socket conn)
                       :output)
   (module/socket-type module
                       (connection/destination-socket conn)
                       :input)))

(defun module/connected? (module conn)
  (quiver/arrow-exists?
   module
   (node-socket/node-index (connection/source-socket conn))
   (node-socket/node-index (connection/destination-socket conn))
   conn))

(defun module/connect! (module conn priority &optional properties)
  (when (and (not (module/connected? module conn))
           (module/connection-possible? module conn))
    (when (quiver/add-arrow!
           module
           (node-socket/node-index (connection/source-socket conn))
           (node-socket/node-index (connection/destination-socket conn))
           conn
           (conn-state~/new priority properties))
      (module~/update-metric module
                             (node-socket/node-index
                              (connection/source-socket conn)))
      t)))

(defun module/disconnect! (module conn)
  (when (quiver/delete-arrow!
         module
         (node-socket/node-index (connection/source-socket conn))
         (node-socket/node-index (connection/destination-socket conn))
         conn)
    (module~/update-metric module
                           (node-socket/node-index
                            (connection/source-socket conn)))
    t))



(defun module/node (module index)
  (let ((node (node+state~/node (module~/node+state module index))))
    (if node
        (node/copy node)
        (error "MODULE/NODE -- no such node with index ~S in the module" index))))
(defun (setf module/node) (new-node module index)
  (setf (node+state~/node (module~/node+state module index)) new-node)
  (dolist (conn (nunion (module/node-input-connections module index)
                        (module/node-output-connections module index)
                        :test #'equal))
    (unless (module/connection-possible? module conn)
      (module/disconnect! module conn)))
  new-node)



(defun module/diagram (module)
  (sort (iterate:iter
          (iterate:for index iterate:in (module/all-nodes module))
          (iterate:collect
              (let ((node-state (module~/node+state module index))
                    (input-conns (module/node-input-connections module index))
                    (output-conns (module/node-output-connections module index)))
                (list (list index (metric~/copy
                                   (node+state~/metric node-state)))
                      input-conns '->
                      (list (node/kind (node+state~/node node-state))
                            (node/name (node+state~/node node-state)))
                      '-> output-conns))))
        #'metric~/test< :key #'(lambda (entry) (second (first entry)))))
