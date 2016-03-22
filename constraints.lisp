;;;; constraint.lisp

(in-package #:cl-gp)

(defparameter *constraints-conjoint-function* (constantly t))

(defun constraints/make-conjoint-function (constraints-list)
  (apply #'alexandria:conjoin constraints-list))
