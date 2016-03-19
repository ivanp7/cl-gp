;;;; properties.lisp

(in-package #:cl-gp)

(defparameter *properties-copy-function* #'copy-list)

(defun make-properties (&rest plist)
  (apply #'list plist))
