;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:iterate)
  :serial t
  :components ((:file "package")
               (:file "graph")
               (:file "type")
               (:file "node")
               (:file "module")
               (:file "program")
               (:file "cl-gp")))
