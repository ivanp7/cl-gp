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
|#

(in-package #:cl-gp)

;;; *** node state ***

(defun node+state~/new (node &optional state)
  (cons node state))

(defun node+state~/node (nstate)
  (car nstate))
(defun (setf node+state~/node) (new-value nstate)
  (setf (car nstate) new-value))

(defun node+state~/state (nstate)
  (cdr nstate))
(defun (setf node+state~/state) (new-value nstate)
  (setf (cdr nstate) new-value))

;;; *** node socket ***

(defun node-socket/new (index field-selector)
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

(defun module/new (name properties)
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
  (quiver/vertex-value (quiver~/vertices module) index))

(defun module/all-nodes (module)
  (quiver/all-vertices module))



(defun module/input-type (module)
  (record/new
   (mapcar #'(lambda (node)
               (field/new (node/name node) (node/output-type node)))
           (delete-if-not #'node/input?
                          (mapcar #'(lambda (index)
                                      (node+state~/node (module~/node+state module index)))
                                  (module/all-nodes module))))))

(defun module/output-type (module)
  (record/new
   (mapcar #'(lambda (node)
               (field/new (node/name node) (node/input-type node)))
           (delete-if-not #'node/output?
                          (mapcar #'(lambda (index)
                                      (node+state~/node (module~/node+state module index)))
                                  (module/all-nodes module))))))



(defun module/add-node! (module node)
  (let ((index (incf (module~/max-index module))))
    (quiver/add-vertex! module index (node+state~/new node))
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
  (quiver/arrow-exists? module
                        (node-socket/node-index (connection/source-socket conn))
                        (node-socket/node-index (connection/destination-socket conn))
                        conn))

(defun module/connect! (module conn priority)
  (when (and (not (module/connected? module conn))
           (module/connection-possible? module conn))
    (quiver/add-arrow! module
                       (node-socket/node-index (connection/source-socket conn))
                       (node-socket/node-index (connection/destination-socket conn))
                       conn
                       (conn-state~/new priority))))

(defun module/disconnect! (module conn)
  (quiver/delete-arrow! module
                        (node-socket/node-index (connection/source-socket conn))
                        (node-socket/node-index (connection/destination-socket conn))
                        conn))



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
