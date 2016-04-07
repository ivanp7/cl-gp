;;;; constraint.lisp

(in-package #:cl-gp)

(defparameter *constraints-conjoint-function* (constantly t))

(defun make-conjoint-constraint-function (functions-list)
  (apply #'alexandria:conjoin functions-list))
