;;;; properties.lisp

(in-package #:cl-gp)

(defparameter *properties-copy-function* #'copy-list)

(defun make-properties (&rest plist)
  (apply #'list plist))

(defun make-conjoint-print-function (fn-list)
  #'(lambda (plist)
      (reduce #'(lambda (str1 str2)
                  (concatenate 'string str1 str2))
              (mapcar #'(lambda (fn)
                          (funcall fn plist))
                      fn-list)
              :initial-value "")))
