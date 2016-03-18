;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-graph
               #:iterate)
  :components ((:file "package")
               (:file "properties" :depends-on ("package"))
               (:file "node" :depends-on ("properties"))
               (:file "connection" :depends-on ("node"))
               (:file "constraint" :depends-on ("package"))
               (:file "module" :depends-on ("properties" "node" "connection" "constraint"))
               (:file "type" :depends-on ("node" "constraint"))
               (:file "interpreter" :depends-on ("module" "type"))
               (:file "cl-gp" :depends-on ("constraint" "module" "interpreter"))))
