;;;; quiver.lisp
;;;; A quiver (multidigraph) implementation

(in-package #:cl-gp)

;;; *** quiver vertex ***

(defun vertex~/new (key value)
  (list key value))

(defun vertex~/key (v)
  (first v))
(defun (setf vertex~/key) (new-value v)
  (setf (first v) new-value))

(defun vertex~/value (v)
  (second v))
(defun (setf vertex~/value) (new-value v)
  (setf (second v) new-value))

;;; *** quiver arrow ***

(defun arrow~/new (key value)
  (list key value))

(defun arrow~/key (e)
  (first e))
(defun (setf arrow~/key) (new-value e)
  (setf (first e) new-value))

(defun arrow~/value (e)
  (second e))
(defun (setf arrow~/value) (new-value e)
  (setf (second e) new-value))

;;; *** arrow group ***

(defun arrowgroup~/new (from to &optional arrow)
  (list (list from to) (if arrow (list arrow))))

(defun arrowgroup~/from (g)
  (first (first g)))
(defun (setf arrowgroup~/from) (new-value g)
  (setf (first (first g)) new-value))

(defun arrowgroup~/to (g)
  (second (first g)))
(defun (setf arrowgroup~/to) (new-value g)
  (setf (second (first g)) new-value))

(defun arrowgroup~/arrows (g)
  (second g))
(defun (setf arrowgroup~/arrows) (new-value g)
  (setf (second g) new-value))

;;; *** quiver ***

(defun quiver/make-empty-quiver (&key (test #'equal))
  (list :test test :v nil :a nil))

(defun quiver~/test (quiver)
  (getf quiver :test #'equal))

(defun quiver~/vertices (quiver)
  (getf quiver :v))
(defun (setf quiver~/vertices) (new-value quiver)
  (setf (getf quiver :v) new-value))

(defun quiver~/arrowgroups (quiver)
  (getf quiver :a))
(defun (setf quiver~/arrowgroups) (new-value quiver)
  (setf (getf quiver :a) new-value))

(defun quiver~/vertex (quiver vertex)
  (find vertex (quiver~/vertices quiver)
        :key #'vertex~/key
        :test (quiver~/test quiver)))

(defun quiver~/parallel-arrows (quiver from to)
  (find-if
   #'(lambda (e)
       (and (funcall (quiver~/test quiver) (arrowgroup~/from e) from)
          (funcall (quiver~/test quiver) (arrowgroup~/to e) to)))
   (quiver~/arrowgroups quiver)))

(defun quiver~/arrow (quiver arrows-group key)
  (find key (arrowgroup~/arrows arrows-group)
        :key #'arrow~/key
        :test (quiver~/test quiver)))

;; 'vertices' = t : copy whole quiver
;; 'except' has a higher priority than 'vertices'
#|
(defun quiver/subquiver (quiver &key (vertices t) except)
  (flet ((preserve? (v)
           (and (not (member v except))
                (or (eql vertices t)
                    (member v vertices)))))
    (let ((vertices (remove-if-not #'preserve?
                                   (quiver~/vertices quiver)
                                   :key #'vertex~/key))
          (arrows (remove-if-not #'(lambda (e)
                                     (and (preserve? (arrow~/from e))
                                          (preserve? (arrow~/to e))))
                                 (quiver~/arrowgroups quiver))))
      (list :v vertices
            :e arrows))))
|#



(defun quiver/all-vertices (quiver)
  (mapcar #'vertex~/key (quiver~/vertices quiver)))

(defun quiver/vertex-member? (quiver vertex)
  (not (null (quiver~/vertex quiver vertex))))

(defun quiver/vertex-arrows (quiver vertex)
  (let ((arrows
         (remove-if-not
          #'(lambda (g)
              (or (funcall (quiver~/test quiver) (arrowgroup~/from g) vertex)
                 (funcall (quiver~/test quiver) (arrowgroup~/to g) vertex)))
          (quiver~/arrowgroups quiver))))
    (mapcar #'(lambda (g)
                (list (list (arrowgroup~/from g)
                            (arrowgroup~/to g))
                      (mapcar #'arrow~/key (arrowgroup~/arrows g))))
            arrows)))

(defun quiver/vertex-inputs (quiver vertex)
  (let ((arrows (remove-if-not
                 #'(lambda (g)
                     (funcall (quiver~/test quiver) (arrowgroup~/to g) vertex))
                 (quiver~/arrowgroups quiver))))
    (mapcar #'(lambda (g)
                (list (list (arrowgroup~/from g)
                            (arrowgroup~/to g))
                      (mapcar #'arrow~/key (arrowgroup~/arrows g))))
            arrows)))

(defun quiver/vertex-outputs (quiver vertex)
  (let ((arrows (remove-if-not
                 #'(lambda (g)
                     (funcall (quiver~/test quiver) (arrowgroup~/from g) vertex))
                 (quiver~/arrowgroups quiver))))
    (mapcar #'(lambda (g)
                (list (list (arrowgroup~/from g)
                            (arrowgroup~/to g))
                      (mapcar #'arrow~/key (arrowgroup~/arrows g))))
            arrows)))

(defun quiver/vertex-value (quiver vertex)
  (let ((v (quiver~/vertex quiver vertex)))
    (if v (vertex~/value v))))
(defun (setf quiver/vertex-value) (new-value quiver vertex)
  (let ((v (quiver~/vertex quiver vertex)))
    (if v
        (setf (vertex~/value v) new-value)
        (error "SETF VERTEX-VALUE -- vertex ~S is not a member of the quiver." vertex))))

(defun quiver/add-vertex! (quiver vertex &optional value)
  (unless (quiver/vertex-member? quiver vertex)
    (push (vertex~/new vertex value) (quiver~/vertices quiver))
    t))

(defun quiver/delete-vertex! (quiver vertex)
  (when (quiver/vertex-member? quiver vertex)
    (setf (quiver~/vertices quiver)
       (delete vertex (quiver~/vertices quiver)
               :key #'vertex~/key
               :test (quiver~/test quiver)))
    (setf (quiver~/arrowgroups quiver)
       (delete-if
        #'(lambda (e)
            (or (funcall (quiver~/test quiver) (arrowgroup~/from e) vertex)
               (funcall (quiver~/test quiver) (arrowgroup~/to e) vertex)))
        (quiver~/arrowgroups quiver)))
    t))



(defun quiver/arrow-exist? (quiver from to key)
  (not (null (quiver~/arrow quiver (quiver~/parallel-arrows quiver from to) key))))

(defun quiver/add-arrow! (quiver from to key &optional value)
  (unless (or (not (quiver/vertex-member? quiver from))
             (not (quiver/vertex-member? quiver to))
             (quiver/arrow-exist? quiver from to key))
    (let ((group (quiver~/parallel-arrows quiver from to)))
      (if group
          (push (arrow~/new key value) (arrowgroup~/arrows group))
          (push (arrowgroup~/new from to (arrow~/new key value))
                (quiver~/arrowgroups quiver))))
    t))

(defun quiver/delete-arrow! (quiver from to key)
  (when (quiver/arrow-exist? quiver from to key)
    (let ((group (quiver~/parallel-arrows quiver from to)))
      (when group
        (setf (arrowgroup~/arrows group)
           (delete key (arrowgroup~/arrows group)
                   :key #'arrow~/key
                   :test (quiver~/test quiver)))
        (unless (arrowgroup~/arrows group)
          (setf (quiver~/arrowgroups quiver)
             (delete-if
              #'(lambda (g)
                  (and (funcall (quiver~/test quiver) (arrowgroup~/from g) from)
                     (funcall (quiver~/test quiver) (arrowgroup~/to g) to)))
              (quiver~/arrowgroups quiver))))
        t))))

(defun quiver/arrow-value (quiver from to key)
  (let ((e (quiver~/arrow quiver (quiver~/parallel-arrows quiver from to) key)))
    (if e (arrow~/value e))))
(defun (setf quiver/arrow-value) (new-value quiver from to key)
  (let ((e (quiver~/arrow quiver (quiver~/parallel-arrows quiver from to) key)))
    (if e
        (setf (arrow~/value e) new-value)
        (error "SETF ARROW-VALUE -- arrow (~S -> ~S : ~S) does not exist." from to key))))
