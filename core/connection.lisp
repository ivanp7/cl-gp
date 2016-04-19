;;;; connection.lisp

(in-package #:cl-gp)

;;; *** selector ***

(defclass abstract-selector ()
  ())

(defgeneric selector/description-string (selector)
  (:documentation "Get selector description string"))

(defgeneric copy-selector (selector)
  (:documentation "Copy selector object"))

(defgeneric selector-equal (selector1 selector2)
  (:documentation "Test if selectors are equal"))

;;; *** arrow ***

(defclass object/arrow ()
  ((source-selector :reader arrow/source-selector
                    :initarg :source-selector
                    :initform nil)
   (target-selector :reader arrow/target-selector
                    :initarg :target-selector
                    :initform nil)))

(defun arrow/description-string (arrow &key no-object-class-name)
  (with-slots (source-selector target-selector) arrow
    (let ((descr (concatenate 'string
                              (selector/description-string source-selector)
                              " -> "
                              (selector/description-string target-selector))))
      (if no-object-class-name
          descr
          (concatenate 'string "ARROW " descr)))))

(defmethod print-object ((instance object/arrow) st)
  (print-unreadable-object (instance st)
    (format st (arrow/description-string instance))))

(defun make-arrow (&key source-selector target-selector)
  (make-instance 'object/arrow
                 :source-selector source-selector
                 :target-selector target-selector))

(defun copy-arrow (arrow)
  (make-arrow :source-selector (copy-selector (arrow/source-selector arrow))
              :target-selector (copy-selector (arrow/target-selector arrow))))

(defun arrow-equal (arrow1 arrow2)
  (or (and (null arrow1) (null arrow2))
     (and arrow1 arrow2
        (selector-equal (arrow/source-selector arrow1)
                        (arrow/source-selector arrow2))
        (selector-equal (arrow/target-selector arrow1)
                        (arrow/target-selector arrow2)))))

;;; *** connection ***

(defclass object/connection (abstract-object)
  ((source :reader connection/source-label
           :initarg :source
           :initform (error "CONNECTION -- :source parameter must be supplied"))
   (target :reader connection/target-label
           :initarg :target
           :initform (error "CONNECTION -- :target parameter must be supplied"))
   (arrow :reader connection/arrow
          :initarg :arrow
          :initform nil)
   (arrow->string-fn :accessor connection/arrow->string
                    :initarg :arrow->string-fn
                    :initform
                    #'(lambda (arrow purpose)
                        (declare (ignore purpose))
                        (if arrow
                            (concatenate 'string
                                         " "
                                         (arrow/description-string
                                          arrow :no-object-class-name t)
                                         " ")
                            " -> ")))))

(defconstant +kind/connection+ 'connection)

(defmethod object/kind ((object object/connection))
  +kind/connection+)

(defun object/connection? (object)
  (kind-equal (object/kind object) +kind/connection+))

(defun connection/regular? (connection)
  (purpose-equal (object/purpose connection) +purpose/regular+))

(define-description-string-method object/connection
    (let ((*print-circle* nil))
      (with-slots (source target arrow purpose properties info-string-fn) object
        (let ((info (funcall info-string-fn object)))
          (concatenate 'string
                       (format nil "~S [~S]~A[~S]"
                               purpose
                               source
                               (funcall (connection/arrow->string object)
                                        arrow purpose)
                               target)
                       (if (plusp (length info)) " " "")
                       info)))))



(defun make-connection (source-label target-label &rest args)
  (~object-init-args-handling-let (+kind/connection+ args)
    (make-object 'object/connection
                 (nconc (list :source source-label
                              :target target-label
                              :properties properties-container
                              :event-handler-fn event-handler-function
                              :info-string-fn info-string-function)
                        (alexandria:delete-from-plist
                         args :source :target :properties :event-handler-fn :info-string-fn)))))

(defun copy-connection (connection &rest args)
  (copy-object connection
               (nconc (list :source (connection/source-label connection)
                            :target (connection/target-label connection)
                            :arrow (let ((arrow (connection/arrow connection)))
                                     (if arrow (copy-arrow arrow)))
                            :arrow->string-fn (connection/arrow->string connection))
                      args)))

(defun connection-equal (conn1 conn2)
  (and (purpose-equal (object/purpose conn1)
                    (object/purpose conn2))
     (label-equal (connection/source-label conn1)
                  (connection/source-label conn2))
     (label-equal (connection/target-label conn1)
                  (connection/target-label conn2))
     (arrow-equal (connection/arrow conn1)
                  (connection/arrow conn2))))

(defparameter +direction/loop+ :loop)
(defparameter +direction/input+ :input)
(defparameter +direction/output+ :output)

(macrolet ((define-function-macro (name expr-both expr-input expr-output expr-none)
             `(defun ,name (connection label)
                (let* ((src-label (connection/source-label connection))
                       (src-label-equal (label-equal label src-label))
                       (tgt-label (connection/target-label connection))
                       (tgt-label-equal (label-equal label tgt-label)))
                  (cond
                    ((and src-label-equal tgt-label-equal) ,expr-both)
                    (tgt-label-equal ,expr-input)
                    (src-label-equal ,expr-output)
                    (t ,expr-none))))))

  (define-function-macro connection/direction
      (values +direction/loop+ label) (values +direction/input+ src-label)
      (values +direction/output+ tgt-label) (values nil nil))

  (define-function-macro connection/other-node-label
      (values label +direction/loop+) (values src-label +direction/input+)
      (values tgt-label +direction/output+) (values nil nil)))

(defun direction/loop? (dir)
  (eql dir +direction/loop+))

(defun direction/input? (dir)
  (eql dir +direction/input+))

(defun direction/output? (dir)
  (eql dir +direction/output+))

(defun direction/inverse (dir)
  (cond
    ((direction/loop? dir) +direction/loop+)
    ((direction/input? dir) +direction/output+)
    ((direction/output? dir) +direction/input+)
    (t nil)))