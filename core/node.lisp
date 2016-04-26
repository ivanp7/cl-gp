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

(defmethod initialize-instance :after ((instance object/node) &key)
  (with-slots (groups) instance
    (setf groups (copy-list groups))))

(defun object/node? (object)
  (typep object 'object/node))

(defun node/regular? (node)
  (purpose-equal (object/purpose node) +purpose/regular+))

(define-description-string-method (object/node 'node)
  (let ((*print-circle* nil))
    (with-slots (label) object
      (format nil "[~S]" label))))

(defun make-node (label &rest args)
  (make-object 'object/node
               (nconc (list :label label)
                      (alexandria:delete-from-plist args :label))))

(defun copy-node (node &optional args)
  (copy-abstract-object
   node (nconc (if (null (getf args :label))
                   (list :label (node/label node)))
               (if (null (getf args :groups))
                   (list :groups (copy-list (node/groups node))))
               args)))

(defmethod copy-object ((object object/node) &rest args)
  (copy-node object args))
