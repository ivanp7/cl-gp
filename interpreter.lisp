;;;; interpreter.lisp

;;; Public functions/constants/variables of the module:
#|
|#

(in-package #:cl-gp)

;;; *** objects names ***

(defparameter *name-print-function*
  #'(lambda (plist)
      (format nil "~S" (getf plist :name))))

(defun node/name (node)
  (getf (node/properties node) :name))

(defun module/name (module)
  (getf (module/properties module) :name))
