;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:iterate)
  :serial t
  :components ((:file "package")
               (:file "quiver")
               (:file "type")
               (:file "node")
               (:file "module")
               (:file "interpreter")
               (:file "cl-gp")))
