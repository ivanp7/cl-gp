(ql:quickload :cl-gp)
(in-package #:cl-gp)

(defparameter *g* (graph/make-empty-graph))
(format t "~S~%" *g*)
(terpri)

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
