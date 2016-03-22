;;;; properties.lisp

(in-package #:cl-gp)

(defun make-properties (&rest plist)
  (apply #'list plist))

(defun copy-properties (properties)
  (copy-list properties))

(defun join-properties (properties-list)
  (reduce #'append properties-list :from-end t))



(defun get-property (properties key &optional default-value)
  (getf properties key default-value))

(defun (setf get-property) (new-value properties key &optional default-value)
  (setf (getf properties key default-value) new-value))



(defun make-conjoint-print-function (fn-list)
  #'(lambda (plist)
      (reduce #'(lambda (str1 str2)
                  (concatenate 'string str1 str2))
              (mapcar #'(lambda (fn)
                          (funcall fn plist))
                      fn-list)
              :initial-value "")))
