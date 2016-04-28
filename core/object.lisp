;;;; object.lisp

(in-package #:cl-gp)

;;; *** abstract object ***

(defparameter *purpose-test* #'eql)

(defun purpose-equal (purpose1 purpose2)
  (funcall *purpose-test* purpose1 purpose2))

(defconstant +purpose/regular+ 'regular)



(defclass abstract-object ()
  ((purpose :reader object/purpose
            :initarg :purpose
            :initform +purpose/regular+)
   (properties :accessor object/properties
               :initarg :properties
               :initform nil)
   (owner :reader object/owner
          :initform nil)
   (constraint-fn-collection :accessor object/constraint-function-collection
                             :initarg :constraint-fn-collection
                             :initform (make-function-collection
                                        #'(lambda (fn-list &optional args)
                                            (every #'(lambda (fn) (apply fn args))
                                               fn-list))))
   (event-handler-collection :accessor object/event-handler-function-collection
                             :initarg :event-handler-collection
                             :initform (make-function-collection))
   (info-string-fn-collection :accessor object/info-string-function-collection
                              :initarg :info-string-fn-collection
                              :initform
                              (make-function-collection
                               #'(lambda (fn-list &optional args)
                                   (if (null fn-list)
                                       ""
                                       (reduce #'(lambda (str1 str2)
                                                   (concatenate 'string str1 " " str2))
                                               (mapcar
                                                #'(lambda (fn)
                                                    (concatenate 'string
                                                                 "{" (apply fn args) "}"))
                                                fn-list))))))))

(defmethod initialize-instance :after ((instance abstract-object) &key)
  (with-slots (properties) instance
    (unless (object/property-collection? properties)
      (setf properties (collect-properties (alexandria:ensure-list properties))))))

(defun abstract-object-class-instance? (object)
  (typep object 'abstract-object))

(defgeneric object/description-string (object &key no-object-class-name)
  (:documentation "Generate description string for printing purposes"))

(defmacro define-description-string-method ((object-class &optional object-class-name)
                                            &body body)
  `(defmethod object/description-string ((object ,object-class) &key no-object-class-name)
     (let ((descr (with-slots (purpose info-string-fn-collection) object
                    (let* ((kind-specific-info (progn ,@body))
                           (extra-info (function-collection/call-all-functions
                                        info-string-fn-collection
                                        (list object)))
                           (info (cond
                                   ((and (plusp (length kind-specific-info))
                                       (plusp (length extra-info)))
                                    (concatenate 'string kind-specific-info " " extra-info))
                                   ((plusp (length kind-specific-info))
                                    kind-specific-info)
                                   ((plusp (length extra-info))
                                    extra-info)
                                   (t ""))))
                      (let ((*print-circle* nil))
                        (if (plusp (length info))
                            (format nil "~S ~A" purpose info)
                            (format nil "~S" purpose)))))))
       (if (not no-object-class-name)
           (let ((*print-circle* nil)
                 (obj-class (if (null ,object-class-name)
                                (type-of object)
                                ,object-class-name)))
             (if (plusp (length descr))
                 (format nil "~S:~A" obj-class descr)
                 (format nil "~S" obj-class)))
           descr))))

(defmethod print-object ((instance abstract-object) st)
  (print-unreadable-object (instance st)
    (let ((*print-circle* nil))
      (format st (object/description-string instance)))))



(labels ((destructively-cut-args-from-plist (key-list plist)
           (iterate:iter
             (for tail initially plist then (cddr tail))
             (while (cddr tail))
             (when (member (caddr tail) key-list)
               (nconcing (list (caddr tail) (cadddr tail)))
               (setf (cddr tail) (cddddr tail)))))
         (make-adjoined-properties-collection (object-class
                                               purpose custom-properties args
                                               functionality-modules)
           (apply #'collect-properties
                  (cons custom-properties
                        (mapcar
                         #'(lambda (module)
                             (multiple-value-bind (property-construction-fn
                                                   applicable-object-init-key-args
                                                   constructed-properties-keys)
                                 (funcall
                                  (functionality-module/properties-constructor-function-getter
                                   module) object-class purpose)
                               (if property-construction-fn
                                   (apply property-construction-fn
                                          (alexandria:mappend
                                           #'(lambda (key)
                                               (let ((property (properties/get-property
                                                                custom-properties key)))
                                                 (if property
                                                     (list key property))))
                                           constructed-properties-keys)
                                          (destructively-cut-args-from-plist
                                           applicable-object-init-key-args args)))))
                         functionality-modules))))
         (add-constraints-and-event-handlers! (object
                                               object-class custom-constraint-fn
                                               custom-event-handler functionality-modules)
           (when custom-constraint-fn
             (function-collection/add-function!
              (object/constraint-function-collection object)
              nil nil custom-constraint-fn))
           (when custom-event-handler
             (function-collection/add-function!
              (object/event-handler-function-collection object)
              nil nil custom-event-handler))
           (dolist (module (reverse functionality-modules))
             (let ((constraint-fn
                    (funcall (functionality-module/constraint-function-getter module)
                             object-class object))
                   (event-handler-fn
                    (funcall (functionality-module/event-handler-function-getter module)
                             object-class object)))
               (if constraint-fn
                   (function-collection/add-function!
                    (object/constraint-function-collection object)
                    (functionality-module/name module) nil constraint-fn))
               (if event-handler-fn
                   (function-collection/add-function!
                    (object/event-handler-function-collection object)
                    (functionality-module/name module) nil event-handler-fn)))))
         (add-info-string-functions! (object
                                      object-class custom-info-string-fn
                                      custom-info-string-fn-first info-string-fn-packages)
           (when (and custom-info-string-fn
                    (not custom-info-string-fn-first))
             (function-collection/add-function!
              (object/info-string-function-collection object)
              nil nil custom-info-string-fn))
           (dolist (pckg info-string-fn-packages)
             (let ((info-string-fn
                    (funcall (info-string-function-package/info-string-function-getter pckg)
                             object-class object)))
               (if info-string-fn
                   (function-collection/add-function!
                    (object/info-string-function-collection object)
                    (info-string-function-package/name pckg) nil info-string-fn))))
           (when (and custom-info-string-fn
                    custom-info-string-fn-first)
             (function-collection/add-function!
              (object/info-string-function-collection object)
              nil nil custom-info-string-fn))))

  (defun make-object (object-class &optional args)
    (let ((args (copy-list args)))
      (let ((functionality-modules
             (getf args :functionality-modules *functionality-modules*))
            (info-string-fn-packages
             (getf args :info-string-function-packages *info-string-function-packages*))
            (custom-constraint-fn (getf args :constraint-fn))
            (custom-event-handler (getf args :event-handler))
            (custom-info-string-fn (getf args :info-string-fn))
            (custom-info-string-fn-first (getf args :custom-info-string-fn-first))
            (custom-properties (collect-properties (getf args :properties)))
            (purpose (getf args :purpose +purpose/regular+))
            (args (nconc (list nil nil)
                         (alexandria:remove-from-plist
                          args :constraint-fn-collection :event-handler-collection
                          :info-string-fn-collection :functionality-modules
                          :info-string-function-packages :constraint-fn :event-handler
                          :info-string-fn :custom-info-string-fn-first :properties
                          :purpose))))
        (let ((properties (make-adjoined-properties-collection
                           object-class purpose custom-properties args functionality-modules)))
          (setf args (cddr args))
          (let ((object (apply (alexandria:curry #'make-instance object-class)
                               (nconc (list :purpose purpose)
                                      (list :properties properties)
                                      args))))
            (add-constraints-and-event-handlers! object object-class custom-constraint-fn
                                                 custom-event-handler functionality-modules)
            (add-info-string-functions! object object-class custom-info-string-fn
                                        custom-info-string-fn-first info-string-fn-packages)
            (function-collection/call-all-functions
             (object/event-handler-function-collection object)
             (list object :on-initialization))
            object))))))

(defun copy-abstract-object (object &optional args)
  (make-object (type-of object)
               (nconc (if (null (getf args :purpose))
                          (list :purpose (object/purpose object)))
                      (if (null (getf args :properties))
                          (list :properties (copy-properties (object/properties object))))
                      (if (null (getf args :constraint-fn-collection))
                          (list :constraint-fn-collection
                                (copy-function-collection
                                 (object/constraint-function-collection object))))
                      (if (null (getf args :event-handler-collection))
                          (list :event-handler-collection
                                (copy-function-collection
                                 (object/event-handler-function-collection object))))
                      (if (null (getf args :info-string-fn-collection))
                          (list :info-string-fn-collection
                                (copy-function-collection
                                 (object/info-string-function-collection object))))
                      args)))

(defgeneric copy-object (object &rest args)
  (:documentation "Make a deep copy of an object"))

(defmethod copy-object ((object abstract-object) &rest args)
  (copy-abstract-object object args))

(defun object/get-property-value (object key &optional default-value)
  (properties/get-property-value (object/properties object) key default-value))

(defun object/set-property-value! (object key new-value)
  (if (object/properties object)
      (properties/set-property-value! (object/properties object) key new-value)
      (progn
        (setf (object/properties object)
           (make-property-collection
            (list (make-property key new-value))))
        new-value)))
