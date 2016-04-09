;;;; constraint-propagation-system.lisp

(in-package #:cl-gp)

;;; *** abstract constraint ***

(defclass cps/constraint ()
  ((value-fn :accessor cps-constraint/inform-about-value-function
             :initarg :value-fn
             :initform (constantly nil))
   (no-value-fn :accessor cps-constraint/inform-about-no-value-function
                :initarg :no-value-fn
                :initform (constantly nil))))

(defun make-cps-constraint (&key value-fn no-value-fn)
  (apply (alexandria:curry #'make-instance 'cps/constraint)
         (nconc (if value-fn (list :value-fn value-fn))
                (if no-value-fn (list :no-value-fn no-value-fn)))))

(defun copy-cps-constraint (cps-constraint)
  (make-cps-constraint
   :value-fn (cps-constraint/inform-about-value-function cps-constraint)
   :no-value-fn (cps-constraint/inform-about-no-value-function cps-constraint)))

;; Inform contraint about a new value on a one of the connectors.
(defun cps-constraint/inform-about-value (constraint)
  (funcall (slot-value constraint 'value-fn)))

;; Inform contraint about a loss of value on a one of the connectors.
(defun cps-constraint/inform-about-no-value (constraint)
  (funcall (slot-value constraint 'no-value-fn)))

;;; *** abstract connector ***

(defparameter *constraint-test* #'eql)

(defclass cps/connector ()
  ((value :reader cps-connector/value
          :initform nil)
   (informant :initform nil)
   (constraints :initform nil)
   (constraint-test-fn :reader cps-connector/constraint-test-function
                       :initarg :constraint-test-fn
                       :initform *constraint-test*)))

(defun cps-connector/has-value? (connector)
  (not (null (slot-value connector 'informant))))

(flet ((for-each-except (exception procedure lst test)
         (dolist (item lst)
           (unless (funcall test item exception)
             (funcall procedure item)))))

  (defun cps-connector/set-value! (connector new-value setter)
    (with-slots (value value-test-fn informant constraints constraint-test-fn) connector
      (unless (cps-connector/has-value? connector)
        (setf value new-value informant setter)
        (for-each-except setter #'cps-constraint/inform-about-value constraints
                         constraint-test-fn)
        t)))

  (defun cps-connector/forget-value! (connector retractor)
    (with-slots (value informant constraints constraint-test-fn) connector
      (when (funcall constraint-test-fn retractor informant)
        (setf value nil)
        (setf informant nil)
        (for-each-except retractor #'cps-constraint/inform-about-no-value constraints
                         constraint-test-fn)
        t))))

;; Tell the connector about a new constraint it belongs to.
(defun cps-connector/connect! (connector new-constraint)
  (with-slots (constraints constraint-test-fn) connector
    (pushnew new-constraint constraints :test constraint-test-fn)
    (when (cps-connector/has-value? connector)
      (cps-constraint/inform-about-value new-constraint))
    t))

(defun cps-connector/disconnect! (connector old-constraint)
  (with-slots (constraints constraint-test-fn) connector
    (setf constraints (delete old-constraint constraints
                           :test constraint-test-fn :count 1))
    (when (cps-connector/has-value? connector)
      (cps-constraint/inform-about-no-value old-constraint))
    t))
