;;;; node.lisp

(in-package #:cl-gp)

(defparameter *label-test* #'eql)

(defun label-equal (label1 label2)
  (funcall *label-test* label1 label2))



(defclass object/node (abstract-object)
  ((label :reader node/label
          :initarg :label
          :initform (error "NODE -- :label parameter must be supplied"))
   (groups :accessor node/groups
           :initarg :groups
           :initform nil)))

(defmethod object/description-string ((object object/node) &key no-object-class-name)
  (let ((descr (let ((*print-circle* nil))
                 (with-slots (label purpose properties info-string-fn) object
                   (let ((info (funcall info-string-fn object)))
                     (concatenate 'string
                                  (format nil "~S [~S]" purpose label)
                                  (if (plusp (length info)) " " "")
                                  info))))))
    (if no-object-class-name
        descr
        (concatenate 'string "NODE " descr))))

(defun node/regular? (node)
  (purpose-equal (object/purpose node) +purpose/regular+))

(defun make-node (label &rest args)
  (make-object 'object/node (nconc (list :label label) args)))

(defun copy-node (node &rest args)
  (copy-object node (nconc (list :label (node/label node)
                                 :groups (copy-list (node/groups node)))
                           args)))
