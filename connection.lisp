;;;; connection.lisp

(in-package #:cl-gp)

;;; *** selector ***

(defun make-selector (tags-list)
  (copy-list tags-list))

(defun copy-selector (selector)
  (copy-list selector))

(defparameter *tag-test* #'eql)

(defun selector-equal (sel1 sel2)
  (and (= (length sel1) (length sel2))
     (every *tag-test* sel1 sel2)))

;;; *** arrow ***

(defclass object/arrow ()
  ((source-selector :reader arrow/source-selector
                    :initarg :source-selector
                    :initform nil)
   (target-selector :reader arrow/target-selector
                    :initarg :target-selector
                    :initform nil)))

(defun arrow/description-string (arrow &key no-object-class)
  (with-slots (source-selector target-selector) arrow
    (let ((descr (let ((*print-circle* nil))
                   (format nil "~:S -> ~:S"
                           source-selector target-selector))))
      (if no-object-class
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
  (and (selector-equal (arrow/source-selector arrow1)
                     (arrow/source-selector arrow2))
     (selector-equal (arrow/target-selector arrow1)
                     (arrow/target-selector arrow2))))

;;; *** connection ***

;; (defconstant +purpose/regular+ :regular) is from node.lisp

(defclass object/connection ()
  ((source :reader connection/source-label
           :initarg :source
           :initform (error "CONNECTION -- :source parameter must be supplied"))
   (target :reader connection/target-label
           :initarg :target
           :initform (error "CONNECTION -- :target parameter must be supplied"))
   (arrow :reader connection/arrow
          :initarg :arrow
          :initform nil)
   (purpose :reader connection/purpose
            :initarg :purpose
            :initform +purpose/regular+)
   (properties :reader connection/properties
               :initarg :properties
               :initform nil)
   (events-handler-fn :accessor connection/events-handler-function
                      :initarg :events-handler-fn
                      :initform (constantly nil))
   (info-string-fn :accessor connection/info-string-function
                   :initarg :info-string-function
                   :initform (constantly ""))
   (arrow->string-fn :accessor connection/arrow->string
                    :initarg :arrow->string-fn
                    :initform
                    #'(lambda (arrow purpose)
                        (declare (ignore purpose))
                        (if arrow
                            (concatenate 'string
                                         " "
                                         (arrow/description-string
                                          arrow :no-object-class t)
                                         " ")
                            " -> ")))))

(defun connection/description-string (connection &key no-object-class)
  (let ((descr (let ((*print-circle* nil))
                 (with-slots (source target arrow purpose properties info-string-fn) connection
                   (let ((info (funcall info-string-fn connection)))
                     (concatenate 'string
                                  (format nil "~S [~S]~A[~S]"
                                          purpose
                                          source
                                          (funcall (connection/arrow->string connection)
                                                   arrow purpose)
                                          target)
                                  (if (plusp (length info)) " " "")
                                  info))))))
    (if no-object-class
        descr
        (concatenate 'string "CONNECTION " descr))))

(defmethod print-object ((instance object/connection) st)
  (print-unreadable-object (instance st)
    (format st (connection/description-string instance))))

(defun connection/regular? (connection)
  (eql (connection/purpose connection) +purpose/regular+))

(defun make-connection (source-label target-label &rest args)
  (apply (alexandria:curry #'make-instance 'object/connection
                           :source source-label
                           :target target-label) args))

(defun copy-connection (connection)
  (make-connection
   (connection/source-label connection)
   (connection/target-label connection)
   :arrow (let ((arrow (connection/arrow connection)))
            (if arrow (copy-arrow arrow)))
   :purpose (connection/purpose connection)
   :properties (copy-properties (connection/properties connection))
   :events-handler-fn (connection/events-handler-function connection)
   :info-string-function (connection/info-string-function connection)
   :arrow->string-fn (connection/arrow->string connection)))

(defun connection-equal (conn1 conn2)
  (and (purpose-equal (connection/purpose conn1)
                    (connection/purpose conn2))
     (label-equal (connection/source-label conn1)
                  (connection/source-label conn2))
     (label-equal (connection/target-label conn1)
                  (connection/target-label conn2))
     (or (and (null (connection/arrow conn1))
           (null (connection/arrow conn2)))
        (arrow-equal (connection/arrow conn1)
                     (connection/arrow conn2)))))

(defun connection/direction (connection label)
  (let* ((src-label (connection/source-label connection))
         (src-label-equal (label-equal label src-label))
         (tgt-label (connection/target-label connection))
         (tgt-label-equal (label-equal label tgt-label)))
    (cond
      ((and src-label-equal tgt-label-equal) (values :loop label))
      (tgt-label-equal (values :input src-label))
      (src-label-equal (values :output tgt-label))
      (t (values nil nil)))))

(defun direction/loop? (dir)
  (eql dir :loop))

(defun direction/input? (dir)
  (eql dir :input))

(defun direction/output? (dir)
  (eql dir :output))
