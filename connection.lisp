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

(defun arrow/description-string (arrow &key no-object-class-name)
  (with-slots (source-selector target-selector) arrow
    (let ((descr (let ((*print-circle* nil))
                   (format nil "~:S -> ~:S"
                           source-selector target-selector))))
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

(defmethod object/description-string ((object object/connection) &key no-object-class-name)
  (let ((descr (let ((*print-circle* nil))
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
                                  info))))))
    (if no-object-class-name
        descr
        (concatenate 'string "CONNECTION " descr))))

(defun connection/regular? (connection)
  (eql (object/purpose connection) +purpose/regular+))

(defun make-connection (source-label target-label &rest args)
  (make-object 'object/connection (nconc (list :source source-label
                                               :target target-label)
                                         args)))

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

(defun connection/direction (connection label)
  (let* ((src-label (connection/source-label connection))
         (src-label-equal (label-equal label src-label))
         (tgt-label (connection/target-label connection))
         (tgt-label-equal (label-equal label tgt-label)))
    (cond
      ((and src-label-equal tgt-label-equal) (values +direction/loop+ label))
      (tgt-label-equal (values +direction/input+ src-label))
      (src-label-equal (values +direction/output+ tgt-label))
      (t (values nil nil)))))

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
