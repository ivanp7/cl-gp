;;;; graph.lisp
;;;; A directed graph implementation

(in-package #:cl-gp)

(defun vertex~/new (key value)
  (list key value))

(defun vertex~/key (v)
  (first v))

(defun vertex~/value (v)
  (second v))
(defun (setf vertex~/value) (new-value v)
  (setf (second v) new-value))



(defun edge~/new (from to value)
  (list from to value))

(defun edge~/from (e)
  (first e))

(defun edge~/to (e)
  (second e))

(defun edge~/value (e)
  (third e))
(defun (setf edge~/value) (new-value e)
  (setf (third e) new-value))



(defun graph/make-empty-graph ()
  (list :v nil :e nil))

(defun graph~/vertices (graph)
  (getf graph :v))
(defun (setf graph~/vertices) (new-value graph)
  (setf (getf graph :v) new-value))

(defun graph~/edges (graph)
  (getf graph :e))
(defun (setf graph~/edges) (new-value graph)
  (setf (getf graph :e) new-value))

;; 'vertices' = t : copy whole graph
;; 'except' has a higher priority than 'vertices'
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

(defun graph/all-vertices (graph)
  (mapcar #'vertex~/key (graph~/vertices graph)))

(defun graph/vertex-exists? (graph vertex)
  (not (null (find vertex (graph~/vertices graph)
                 :key #'vertex~/key))))

(defun graph/vertex-direct-successors (graph vertex)
  (mapcar #'edge~/to
          (remove-if-not
           #'(lambda (e) (eql (edge~/from e) vertex))
           (graph~/edges graph))))

(defun graph/vertex-direct-predecessors (graph vertex)
  (mapcar #'edge~/from
          (remove-if-not
           #'(lambda (e) (eql (edge~/to e) vertex))
           (graph~/edges graph))))

(defun graph/vertices-adjacent? (graph from-vertex to-vertex)
  (not (null (find-if #'(lambda (e)
                        (and (eql (edge~/from e) from-vertex)
                           (eql (edge~/to e) to-vertex)))
                    (graph~/edges graph)))))

(defun graph/add-vertex! (graph vertex &optional value)
  (unless (graph/vertex-exists? graph vertex)
    (push (vertex~/new vertex value) (graph~/vertices graph))
    t))

(defun graph/delete-vertex! (graph vertex)
  (when (graph/vertex-exists? graph vertex)
    (setf (graph~/vertices graph)
       (delete vertex (graph~/vertices graph)
               :key #'vertex~/key))
    (setf (graph~/edges graph)
       (delete-if #'(lambda (e)
                      (or (eql (edge~/from e) vertex)
                         (eql (edge~/to e) vertex)))
                  (graph~/edges graph)))
    t))

(defun graph/add-edge! (graph from-vertex to-vertex &optional value)
  (unless (or (not (graph/vertex-exists? graph from-vertex))
             (not (graph/vertex-exists? graph to-vertex))
             (graph/vertices-adjacent? graph from-vertex to-vertex))
    (push (edge~/new from-vertex to-vertex value) (graph~/edges graph))
    t))

(defun graph/delete-edge! (graph from-vertex to-vertex)
  (when (graph/vertices-adjacent? graph from-vertex to-vertex)
    (setf (graph~/edges graph)
       (delete-if #'(lambda (e)
                      (and (eql (edge~/from e) from-vertex)
                         (eql (edge~/to e) to-vertex)))
                  (graph~/edges graph)))
    t))

(defun graph/vertex-value (graph vertex)
  (let ((v (find vertex (graph~/vertices graph)
                 :key #'vertex~/key)))
    (if v (vertex~/value v))))
(defun (setf graph/vertex-value) (new-value graph vertex)
  (let ((v (find vertex (graph~/vertices graph)
                 :key #'vertex~/key)))
    (if v (setf (vertex~/value v) new-value))))

(defun graph/edge-value (graph from-vertex to-vertex)
  (let ((e (find-if #'(lambda (e)
                        (and (eql (edge~/from e) from-vertex)
                           (eql (edge~/to e) to-vertex)))
                    (graph~/vertices graph))))
    (if e (edge~/value e))))
(defun (setf graph/edge-value) (new-value graph from-vertex to-vertex)
  (let ((e (find-if #'(lambda (e)
                        (and (eql (edge~/from e) from-vertex)
                           (eql (edge~/to e) to-vertex)))
                    (graph~/vertices graph))))
    (if e (setf (edge~/value e) new-value))))
