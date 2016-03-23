;;;; constraint.lisp

(in-package #:cl-gp)

(defparameter *constraints-conjoint-function* (constantly t))

(defun make-conjoint-function (functions-list)
  (apply #'alexandria:conjoin functions-list))

(defun make-sequence-function (functions-list)
  #'(lambda (&rest args)
      (dolist (fn functions-list)
        (apply fn args))
      nil))
