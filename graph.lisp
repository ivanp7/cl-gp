;;;; graph.lisp
;;;; A directed graph implementation

(in-package #:cl-gp)

;;; *** graph vertex ***

(defun vertex~/new (key value inputs outputs)
  (list key value inputs outputs))

(defun vertex~/key (v)
  (first v))
(defun (setf vertex~/key) (new-value v)
  (setf (first v) new-value))

(defun vertex~/value (v)
  (second v))
(defun (setf vertex~/value) (new-value v)
  (setf (second v) new-value))

(defun vertex~/inputs (v)
  (third v))
(defun (setf vertex~/inputs) (new-value v)
  (setf (third v) new-value))

(defun vertex~/outputs (v)
  (fourth v))
(defun (setf vertex~/outputs) (new-value v)
  (setf (fourth v) new-value))

;;; *** graph edge ***

(defun edge~/new (from-vertex from-output to-vertex to-input value)
  (list (list from-vertex from-output) (list to-vertex to-input) value))

(defun edge~/from-vertex (e)
  (first (first e)))
(defun (setf edge~/from-vertex) (new-value e)
  (setf (first (first e)) new-value))

(defun edge~/from-output (e)
  (second (first e)))
(defun (setf edge~/from-output) (new-value e)
  (setf (second (first e)) new-value))

(defun edge~/to-vertex (e)
  (first (second e)))
(defun (setf edge~/to-vertex) (new-value e)
  (setf (first (second e)) new-value))

(defun edge~/to-input (e)
  (second (second e)))
(defun (setf edge~/to-input) (new-value e)
  (setf (second (second e)) new-value))

(defun edge~/value (e)
  (third e))
(defun (setf edge~/value) (new-value e)
  (setf (third e) new-value))

;;; *** graph ***

(defun graph/make-empty-graph (&optional (test #'equal))
  (list :test test :v nil :e nil))

(defun graph~/test (graph)
  (getf graph :test #'equal))

(defun graph~/vertices (graph)
  (getf graph :v))
(defun (setf graph~/vertices) (new-value graph)
  (setf (getf graph :v) new-value))

(defun graph~/edges (graph)
  (getf graph :e))
(defun (setf graph~/edges) (new-value graph)
  (setf (getf graph :e) new-value))

(defun graph~/vertex (graph vertex)
  (find vertex (graph~/vertices graph)
        :key #'vertex~/key
        :test (graph~/test graph)))

(defun graph~/edge (graph from-vertex from-output to-vertex to-input)
  (find-if
   #'(lambda (e)
       (and (funcall (graph~/test graph)
                   (edge~/from-vertex e) from-vertex)
          (funcall (graph~/test graph)
                   (edge~/from-output e) from-output)
          (funcall (graph~/test graph)
                   (edge~/to-vertex e) to-vertex)
          (funcall (graph~/test graph)
                   (edge~/to-input e) to-input)))
   (graph~/edges graph)))

;; 'vertices' = t : copy whole graph
;; 'except' has a higher priority than 'vertices'
#|
(defun graph/subgraph (graph &key (vertices t) except)
  (flet ((preserve? (v)
           (and (not (member v except))
                (or (eql vertices t)
                    (member v vertices)))))
    (let ((vertices (remove-if-not #'preserve?
                                   (graph~/vertices graph)
                                   :key #'vertex~/key))
          (edges (remove-if-not #'(lambda (e)
                                    (and (preserve? (edge~/from e))
                                         (preserve? (edge~/to e))))
                                (graph~/edges graph))))
      (list :v vertices
            :e edges))))
|#



(defun graph/all-vertices (graph)
  (mapcar #'vertex~/key (graph~/vertices graph)))

(defun graph/vertex-exists? (graph vertex)
  (not (null (graph~/vertex graph vertex))))

(defun graph/vertex-inputs (graph vertex)
  (copy-list (vertex~/inputs (graph~/vertex graph vertex))))

(defun graph/vertex-outputs (graph vertex)
  (copy-list (vertex~/outputs (graph~/vertex graph vertex))))

(defun graph/vertex-value (graph vertex)
  (let ((v (graph~/vertex graph vertex)))
    (if v (vertex~/value v))))
(defun (setf graph/vertex-value) (new-value graph vertex)
  (let ((v (graph~/vertex graph vertex)))
    (if v (setf (vertex~/value v) new-value))))

(defun graph/add-vertex! (graph vertex &key value inputs outputs)
  (unless (or (graph/vertex-exists? graph vertex)
             (not (listp inputs))
             (/= (length inputs)
                (length (remove-duplicates inputs
                                           :test (graph~/test graph))))
             (not (listp outputs))
             (/= (length outputs)
                (length (remove-duplicates outputs
                                           :test (graph~/test graph)))))
    (push (vertex~/new vertex value (copy-list inputs) (copy-list outputs))
          (graph~/vertices graph))
    t))

(defun graph/delete-vertex! (graph vertex)
  (when (graph/vertex-exists? graph vertex)
    (setf (graph~/vertices graph)
       (delete vertex (graph~/vertices graph)
               :key #'vertex~/key
               :test (graph~/test graph)))
    (setf (graph~/edges graph)
       (delete-if #'(lambda (e)
                      (or (funcall (graph~/test graph) (edge~/from-vertex e) vertex)
                         (funcall (graph~/test graph) (edge~/to-vertex e) vertex)))
                  (graph~/edges graph)))
    t))



(defun graph/vertex-direct-successors (graph vertex output)
  (mapcar #'(lambda (e) (list (edge~/to-vertex e)
                         (edge~/to-input e)))
          (remove-if-not
           #'(lambda (e) (and (funcall (graph~/test graph) (edge~/from-vertex e) vertex)
                       (funcall (graph~/test graph) (edge~/from-output e) output)))
           (graph~/edges graph))))

(defun graph/vertex-direct-predecessors (graph vertex input)
  (mapcar #'(lambda (e) (list (edge~/from-vertex e)
                         (edge~/from-output e)))
          (remove-if-not
           #'(lambda (e) (and (funcall (graph~/test graph) (edge~/to-vertex e) vertex)
                       (funcall (graph~/test graph) (edge~/to-input e) input)))
           (graph~/edges graph))))

(defun graph/vertices-adjacent? (graph from-vertex from-output to-vertex to-input)
  (not (null (graph~/edge graph from-vertex from-output to-vertex to-input))))

(defun graph/add-edge! (graph from-vertex from-output to-vertex to-input &optional value)
  (unless (or (not (graph/vertex-exists? graph from-vertex))
             (not (find from-output (graph/vertex-outputs graph from-vertex)
                      :test (graph~/test graph)))
             (not (graph/vertex-exists? graph to-vertex))
             (not (find to-input (graph/vertex-inputs graph to-vertex)
                      :test (graph~/test graph)))
             (graph/vertices-adjacent? graph from-vertex from-output to-vertex to-input))
    (push (edge~/new from-vertex from-output to-vertex to-input value)
          (graph~/edges graph))
    t))

(defun graph/delete-edge! (graph from-vertex from-output to-vertex to-input)
  (when (graph/vertices-adjacent? graph from-vertex from-output to-vertex to-input)
    (setf (graph~/edges graph)
       (delete-if #'(lambda (e)
                      (and (funcall (graph~/test graph)
                                  (edge~/from-vertex e) from-vertex)
                         (funcall (graph~/test graph)
                                  (edge~/from-output e) from-output)
                         (funcall (graph~/test graph)
                                  (edge~/to-vertex e) to-vertex)
                         (funcall (graph~/test graph)
                                  (edge~/to-input e) to-input)))
                  (graph~/edges graph)))
    t))

(defun graph/edge-value (graph from-vertex from-output to-vertex to-input)
  (let ((e (graph~/edge graph from-vertex from-output to-vertex to-input)))
    (if e (edge~/value e))))
(defun (setf graph/edge-value) (new-value graph from-vertex from-output to-vertex to-input)
  (let ((e (graph~/edge graph from-vertex from-output to-vertex to-input)))
    (if e (setf (edge~/value e) new-value))))
