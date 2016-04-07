;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-graph
               #:iterate)
  :components ((:file "package")
               (:file "object" :depends-on ("package"))
               (:file "node" :depends-on ("object"))
               (:file "connection" :depends-on ("object" "node"))
               (:file "constraints" :depends-on ("package"))
               (:file "graph" :depends-on ("object" "node" "connection" "constraints"))
               (:file "uses" :depends-on ("graph"))
               (:file "call-constraint" :depends-on ("uses"))
               (:file "type-constraint" :depends-on ("uses"))
               (:file "feedforward-constraint" :depends-on ("uses"))
               (:file "disjoint-inputs-constraint" :depends-on ("uses"))
               (:file "finite-recursion-constraint" :depends-on ("uses"))
               (:file "interpreter" :depends-on ("uses"))
               (:file "cl-gp" :depends-on ("constraints" "graph" "interpreter"))))
