;;;; quiver.lisp
;;;; A quiver (multidigraph) implementation

(in-package #:cl-gp)

;;; *** quiver arrow ***

(defun arrow~/new (key value)
  (cons key value))

(defun arrow~/key (e)
  (car e))
(defun (setf arrow~/key) (new-value e)
  (setf (car e) new-value))

(defun arrow~/value (e)
  (cdr e))
(defun (setf arrow~/value) (new-value e)
  (setf (cdr e) new-value))

;;; *** arrow direction ***

(defun arrow-direction~/new (from to)
  (cons from to))

(defun arrow-direction~/from (dir)
  (car dir))
(defun (setf arrow-direction~/from) (new-value dir)
  (setf (car dir) new-value))

(defun arrow-direction~/to (dir)
  (cdr dir))
(defun (setf arrow-direction~/to) (new-value dir)
  (setf (cdr dir) new-value))

;;; *** arrow group ***

(defun parallel-arrows-group~/copy (group)
  (copy-alist group))

(defun parallel-arrows-group~/add-arrow! (group arrow)
  (cons arrow group))

(defun parallel-arrows-group~/delete-arrow! (group key test)
  (delete key group :key #'arrow~/key :test test))

(defun parallel-arrows-group~/find-arrow (group key test)
  (find key group :key #'arrow~/key :test test))

;;; *** quiver ***

(defun quiver/make-empty-quiver (&key properties (test #'equal))
  (list :properties properties
        :v (make-hash-table :test test)
        :a (make-hash-table
            :test #'(lambda (key1 key2)
                      (and (funcall test
                                  (arrow-direction~/from key1)
                                  (arrow-direction~/from key2))
                         (funcall test
                                  (arrow-direction~/to key1)
                                  (arrow-direction~/to key2)))))))

(defun quiver/properties (quiver)
  (getf quiver :properties))
(defun (setf quiver/properties) (new-value quiver)
  (setf (getf quiver :properties) new-value))

(defun quiver~/test (quiver)
  (hash-table-test (getf quiver :v)))

(defun quiver~/vertices (quiver)
  (getf quiver :v))

(defun quiver~/parallel-arrows-groups (quiver)
  (getf quiver :a))

(defun quiver~/parallel-arrows-group (quiver from to)
  (gethash (arrow-direction~/new from to)
           (quiver~/parallel-arrows-groups quiver)))
(defun (setf quiver~/parallel-arrows-group) (new-value quiver from to)
  (setf (gethash (arrow-direction~/new from to)
              (quiver~/parallel-arrows-groups quiver)) new-value))



(defmacro quiver~/vertex-arrows-getter (selector)
  `(let (groups (test (quiver~/test quiver)))
     (maphash #'(lambda (key value)
                  (when ,selector
                    (push (list :from (arrow-direction~/from key)
                                :to (arrow-direction~/to key)
                                :group (parallel-arrows-group~/copy value))
                          groups)))
              (quiver~/arrowgroups quiver))
     groups))

(defun quiver/vertex-arrows (quiver vertex)
  (quiver~/vertex-arrows-getter
   (or (funcall test (arrow-direction~/from key) vertex)
      (funcall test (arrow-direction~/to key) vertex))))

(defun quiver/vertex-inputs (quiver vertex)
  (quiver~/vertex-arrows-getter
   (and (not (funcall test (arrow-direction~/from key) vertex))
      (funcall test (arrow-direction~/to key) vertex))))

(defun quiver/vertex-outputs (quiver vertex)
  (quiver~/vertex-arrows-getter
   (and (funcall test (arrow-direction~/from key) vertex)
      (not (funcall test (arrow-direction~/to key) vertex)))))

(defun quiver/vertex-loops (quiver vertex)
  (quiver~/vertex-arrows-getter
   (and (funcall test (arrow-direction~/from key) vertex)
      (funcall test (arrow-direction~/to key) vertex))))



(defun quiver/all-vertices (quiver)
  (alexandria:hash-table-keys (quiver~/vertices quiver)))

(defun quiver/vertex-member? (quiver vertex)
  (nth-value 1 (gethash vertex (quiver~/vertices quiver))))

(defun quiver/vertex-value (quiver vertex)
  (gethash vertex (quiver~/vertices quiver)))
(defun (setf quiver/vertex-value) (new-value quiver vertex)
  (if (quiver/vertex-member? quiver vertex)
      (setf (gethash vertex (quiver~/vertices quiver)) new-value)
      (error "SETF VERTEX-VALUE -- no such vertex ~S is in the quiver." vertex)))

(defun quiver/add-vertex! (quiver vertex &optional value)
  (unless (quiver/vertex-member? quiver vertex)
    (setf (gethash vertex (quiver~/vertices quiver)) value)
    t))

(defun quiver/delete-vertex! (quiver vertex)
  (when (remhash vertex (quiver~/vertices quiver))
    (let ((test (quiver~/test quiver)))
      (alexandria:maphash-keys
       #'(lambda (key)
           (if (or (funcall test (arrow-direction~/from key) vertex)
                  (funcall test (arrow-direction~/to key) vertex))
               (remhash key (quiver~/parallel-arrows-groups quiver))))
       (quiver~/parallel-arrows-groups quiver)))
    t))



(defun quiver/arrow-exist? (quiver from to key)
  (not (null (parallel-arrows-group~/find-arrow
            (quiver~/parallel-arrows-group quiver from to)
            key (quiver~/test quiver)))))

(defun quiver/add-arrow! (quiver from to key &optional value)
  (when (and (quiver/vertex-member? quiver from)
           (quiver/vertex-member? quiver to)
           (not (quiver/arrow-exist? quiver from to key)))
    (setf (quiver~/parallel-arrows-group quiver from to)
       (parallel-arrows-group~/add-arrow!
        (quiver~/parallel-arrows-group quiver from to)
        (arrow~/new key value)))
    t))

(defun quiver/delete-arrow! (quiver from to key)
  (when (quiver/arrow-exist? quiver from to key)
    (setf (quiver~/parallel-arrows-group quiver from to)
       (parallel-arrows-group~/delete-arrow!
        (quiver~/parallel-arrows-group quiver from to)
        key (quiver~/test quiver)))
    (if (null (quiver~/parallel-arrows-group quiver from to))
        (remhash (arrow-direction~/new from to)
                 (quiver~/parallel-arrows-group quiver from to)))
    t))

(defun quiver/arrow-value (quiver from to key)
  (arrow~/value (parallel-arrows-group~/find-arrow
                 (quiver~/parallel-arrows-group quiver from to)
                 key (quiver~/test quiver))))
(defun (setf quiver/arrow-value) (new-value quiver from to key)
  (if (quiver/arrow-exist? quiver from to key)
      (setf (arrow~/value (parallel-arrows-group~/find-arrow
                        (quiver~/parallel-arrows-group quiver from to)
                        key (quiver~/test quiver)))
         new-value)
      (error "SETF ARROW-VALUE -- no such arrow (~S -> ~S):~S is in the quiver."
             from to key)))
