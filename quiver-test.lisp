
(in-package #:cl-gp)

(defparameter *q* (quiver/make-empty-quiver))

(quiver/add-vertex! *q* 'v1 1)
(quiver/add-vertex! *q* 'v2 2)
(quiver/add-vertex! *q* 'v3 3)

(quiver/add-arrow! *q* 'v1 'v2 'a0 0)
(quiver/add-arrow! *q* 'v1 'v2 'a1 -1)
(quiver/add-arrow! *q* 'v2 'v1 'a2 -2)
(quiver/add-arrow! *q* 'v1 'v3 'a3 -3)
(quiver/add-arrow! *q* 'v2 'v3 'a4 -4)

(format t "~S~%" *q*) (terpri)

(quiver/delete-arrow! *q* 'v2 'v3 'a4)

(format t "~S~%" *q*) (terpri)

(quiver/delete-vertex! *q* 'v3)

(format t "~S~%" *q*) (terpri)

(format t "~S~%" (quiver/vertex-arrows *q* 'v1)) (terpri)

#|
(defparameter *g* (graph/make-empty-graph))

(graph/add-vertex! *g* 1 :value '(primitive if) :inputs '(cnd cns alt) :outputs '(result))
(graph/add-vertex! *g* 2 :value '(primitive 1) :outputs '(value))
(graph/add-vertex! *g* 3 :value '(primitive =) :inputs '(val1 val2) :outputs '(result))
(graph/add-vertex! *g* 4 :value '(primitive 0) :outputs '(value))
(graph/add-vertex! *g* 5 :value 'input :outputs '(value))
(graph/add-vertex! *g* 6 :value '(primitive *) :inputs '(val1 val2) :outputs '(result))
(graph/add-vertex! *g* 7 :value '(module factorial) :inputs '(n) :outputs '(result))
(graph/add-vertex! *g* 8 :value '(primitive -) :inputs '(val1 val2) :outputs '(result))
;;(graph/add-vertex! *g* 9 :value '(primitive 1) :outputs '(value))
(graph/add-vertex! *g* 10 :value 'output :inputs '(value))

(graph/add-edge! *g* 1 'result 10 'value)
(graph/add-edge! *g* 2 'value 1 'cns)
(graph/add-edge! *g* 3 'result 1 'cnd)
(graph/add-edge! *g* 4 'value 3 'val1)
(graph/add-edge! *g* 5 'value 3 'val2)
(graph/add-edge! *g* 6 'result 1 'alt)
(graph/add-edge! *g* 5 'value 6 'val1)
(graph/add-edge! *g* 7 'result 6 'val2)
(graph/add-edge! *g* 8 'result 7 'n)
(graph/add-edge! *g* 5 'value 8 'val1)
(graph/add-edge! *g* 2 'value 8 'val2)

(format t "~S~%" *g*)
(terpri)

(format t "~S~%" (graph/vertex-direct-successors *g* 5 'value))
(format t "~S~%" (graph/vertex-direct-successors *g* 2 'value))
(format t "~S~%" (graph/vertex-direct-predecessors *g* 6 'val1))
(format t "~S~%" (graph/vertex-value *g* 7))
(terpri)

(graph/delete-vertex! *g* 1)

(format t "~S~%" *g*)
(terpri)
|#
#|
(format t "~S ~S~%" (graph/add-vertex! *g* 1 'a) *g*)
(format t "~S ~S~%" (graph/add-vertex! *g* 2 'b) *g*)
(format t "~S ~S~%" (graph/add-vertex! *g* 3 'c) *g*)
(terpri)

(format t "~S ~S~%" (graph/add-edge! *g* 1 2 '(a b)) *g*)
(format t "~S ~S~%" (graph/add-edge! *g* 1 3 '(a c)) *g*)
(format t "~S ~S~%" (graph/add-edge! *g* 4 3 '(d c)) *g*)
(terpri)

(format t "~S~%" (graph/vertex-direct-successors *g* 1))
(terpri)

(format t "~S~%" (graph/vertices-adjacent? *g* 1 2))
(format t "~S~%" (graph/vertices-adjacent? *g* 2 1))
(terpri)

(format t "~S~%" (graph/vertex-value *g* 1))
(format t "~S~%" (setf (graph/vertex-value *g* 1) 'aa))
(format t "~S~%" (graph/vertex-value *g* 1))
(terpri)

(format t "~S ~S~%" (graph/delete-edge! *g* 1 2) *g*)
(format t "~S ~S~%" (graph/delete-vertex! *g* 1) *g*)
(terpri)
|#
