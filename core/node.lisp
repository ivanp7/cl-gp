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

(defconstant +kind/node+ 'node)

(defmethod object/kind ((object object/node))
  +kind/node+)

(defun object/node? (object)
  (kind-equal (object/kind object) +kind/node+))

(defun node/regular? (node)
  (purpose-equal (object/purpose node) +purpose/regular+))

(define-description-string-method object/node
  (let ((*print-circle* nil))
    (with-slots (label purpose properties info-string-fn) object
      (let ((info (funcall info-string-fn object)))
        (concatenate 'string
                     (format nil "~S [~S]" purpose label)
                     (if (plusp (length info)) " " "")
                     info)))))

(defun make-node (label &rest args)
  (~object-init-args-handling-let (+kind/node+ args)
    (make-object 'object/node
                 (nconc (list :label label
                              :properties properties-container
                              :event-handler-fn event-handler-function
                              :info-string-fn info-string-function)
                        (alexandria:delete-from-plist
                         args :label :properties :event-handler-fn :info-string-fn)))))

(defun copy-node (node &rest args)
  (copy-object node (nconc (list :label (node/label node)
                                 :groups (copy-list (node/groups node)))
                           args)))
