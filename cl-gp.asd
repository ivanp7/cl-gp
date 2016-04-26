;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :description "Implementation of genetic programming algorithm."
  :author "ivanp7 <ivanp7@mail.ru>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-graph
               #:iterate)
  :components
  (;; library core
   (:file "package")
   (:file "core/constraint-propagation-system" :depends-on ("package"))
   (:file "core/property" :depends-on ("core/constraint-propagation-system"))
   (:file "core/functionality-module" :depends-on ("package"))
   (:file "core/object" :depends-on ("core/property" "core/functionality-module"))
   (:file "core/base" :depends-on ("core/object"))

   (:file "core/node" :depends-on ("core/base"))
   (:file "core/connection" :depends-on ("core/base"
                                         "core/node"))
   (:file "core/graph" :depends-on ("core/base"
                                    "core/node"
                                    "core/connection"))
   (:file "core/core" :depends-on ("core/graph"))

   ;; functionality modules
   (:file "functionality-modules/reference/reference"
          :depends-on ("core/core"))

   (:file "functionality-modules/name-property/name"
          :depends-on ("core/core"
                       "functionality-modules/reference/reference"))

   (:file "functionality-modules/strong-typing-constraint/type-entity"
          :depends-on ("package"))
   (:file "functionality-modules/strong-typing-constraint/constraint"
          :depends-on ("core/core"
                       "functionality-modules/reference/reference"
                       "functionality-modules/strong-typing-constraint/type-entity"))

   ;; applications
   (:file "applications/application" :depends-on ("core/core"))

   (:file "applications/programs/program"
          :depends-on ("applications/application"
                       "functionality-modules/reference/reference"
                       "functionality-modules/name-property/name"))

   ;; algorithm
   (:file "genetic-programming/algorithm" :depends-on ("package"))))
