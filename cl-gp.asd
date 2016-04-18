;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-graph
               #:iterate)
  :components (;; abstract graph framework
               (:file "package")
               (:file "object" :depends-on ("package"))
               (:file "structural-constraints" :depends-on ("package"))
               (:file "node" :depends-on ("object" "structural-constraints"))
               (:file "connection" :depends-on ("object" "node" "structural-constraints"))
               (:file "constraint-propagation-system" :depends-on ("package"))
               (:file "graph" :depends-on ("object" "node" "connection"
                                                    "structural-constraints"))
               (:file "reference" :depends-on ("graph"))


               ;; generic constraints:
               ;;(:file "strong-typing-constraint"
               ;;       :depends-on ("graph" "reference" "constraint-propagation-system"))


               ;; specific methods of use:
               ;; 1) programs
               (:file "programs" :depends-on ("graph" "reference"))


               ;; abstraction over methods of use
               ;;(:file "use" :depends-on ("programs"))


               ;; abstract genetic programming algorithm
               ;;(:file "fitness" :depends-on ("use"))
               ;;(:file "operators" :depends-on ("use"))
               ;;(:file "cl-gp" :depends-on ("graph" "fitness" "operators"))
               )
  #+nil (;; abstract graph framework
       (:file "package")
       (:file "object" :depends-on ("package"))
       (:file "structural-constraints" :depends-on ("package"))
       (:file "node" :depends-on ("object" "structural-constraints"))
       (:file "connection" :depends-on ("object" "node" "structural-constraints"))
       (:file "constraint-propagation-system" :depends-on ("package"))
       (:file "graph" :depends-on ("object" "node" "connection"
                                            "structural-constraints"))
       (:file "reference" :depends-on ("graph"))


       ;; generic constraints:
       (:file "disjoint-input-arrows-constraint" :depends-on ("graph"))
       (:file "strong-typing-constraint"
              :depends-on ("graph" "reference" "constraint-propagation-system"))


       ;; specific methods of use:
       ;; 1) programs
       (:file "programs" :depends-on ("graph"
                                      "reference"
                                      "disjoint-input-arrows-constraint"))


       ;; abstraction over methods of use
       (:file "use" :depends-on ("programs"))


       ;; abstract genetic programming algorithm
       (:file "fitness" :depends-on ("use"))
       (:file "operators" :depends-on ("use"))
       (:file "cl-gp" :depends-on ("graph" "fitness" "operators"))))
