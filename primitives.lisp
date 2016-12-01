;;;; primitives.lisp

(in-package #:sketcheroo)

(define-condition geometry-error (simple-error) ())

(defun geometry-error (message-or-control &rest arguments)
  (error 'geometry-error
         :format-control (if arguments "~{~}" "~A")
         :format-arguments (list* message-or-control arguments)))

(defgeneric combine (object1 object2)
  (:documentation "Return the combination of two objects."))

(defgeneric displace (object1 object2)
  (:documentation "Return a new object matching the type of OBJECT1
  but displaced by OBJECT2."))

(defgeneric add (object1 object2))

(defgeneric sub (object1 object2))

(defgeneric mul (object1 object2))

(defgeneric div (object1 object2))

(defgeneric neg (object1))

(defgeneric eqv (object1 object2)
  (:documentation "Return true if OBJECT1 is equivalent to OBJECT2."))

(defgeneric bounding-rect (object)
  (:documentation "Return the bounding rectangle of OBJECT.")
  (:method ((object sequence))
    (reduce #'combine object)))

(defgeneric rect-canvas (rect)
  (:documentation "Return the canvas needed to fully enclose RECT."))

(defgeneric expand (rect amount)
  (:documentation "Expand RECT by AMOUNT, in every direction."))

(defgeneric contract (rect amount)
  (:documentation "Contract RECT by AMOUNT, in every direction."))

(defmethod point-designator (object))

(defgeneric x (object)
  (:method (object)
    (x (point-designator object))))

(defgeneric y (object)
  (:method (object)
    (y (point-designator object))))

(defgeneric width (object))

(defgeneric height (object))

(defgeneric magnitude (object))

(defgeneric minpoint (object)
  (:documentation "Return the point of OBJECT that is closest to
  negative infinity on both axes."))

(defgeneric maxpoint (object)
  (:documentation "Return the point of OBJECT that is closest to
  positive infinity on both axes."))

(defgeneric centerpoint (object)
  (:documentation "Return the point at the center of OBJECT."))

(defgeneric midpoint (object1 object2))

(defgeneric xmin (object)
  (:documentation "Return the X component of the minimum point of
  OBJECT."))

(defgeneric ymin (object)
  (:documentation "Return the Y component of the minimum point of OBJECT."))

(defgeneric xmax (object)
  (:documentation "Return the X component of the maximum point of OBJECT."))

(defgeneric ymax (object)
  (:documentation "Return the Y component of the maximum point of OBJECT."))

(defclass geometric () ())

(defmethod pretty-components (object))

(defmethod print-object ((object geometric) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (pretty-components object))))

(defclass point (geometric)
  ((x
    :initarg :x
    :reader x)
   (y
    :initarg :y
    :reader y)))

(defmethod pretty-components ((point point))
  (format nil "~A,~A" (x point) (y point)))

(defmethod copy-initargs ((point point))
  (list :x (x point) :y (y point)))

(defmethod eqv ((p1 point) (p2 point))
  (and (= (x p1) (x p2))
       (= (y p1) (y p2))))

(defgeneric point (x y)
  (:method (x y)
    (make-instance 'point :x x :y y)))

(defgeneric xpoint (x)
  (:documentation "Return a point with X as its X component and 0 as
  its Y component.")
  (:method (x)
    (point x 0)))

(defgeneric ypoint (y)
  (:documentation "Return a point with 0 as its X component and Y as
  its Y component.")
  (:method (y)
    (point 0 y)))

(defgeneric spoint (i)
  (:documentation "Return a 'symmetric' point that has I as its X and
  Y component.")
  (:method (i)
    (point i i)))

(defmethod minpoint ((point point))
  point)

(defmethod maxpoint ((point point))
  point)

(defmethod centerpoint ((point point))
  point)

(defmethod add ((p1 point) (p2 point))
  (point (+ (x p1) (x p2))
         (+ (y p1) (y p2))))

(defmethod sub ((p1 point) (p2 point))
  (point (- (x p1) (x p2))
         (- (y p1) (y p2))))

(defmethod mul ((p1 point) (p2 point))
  (point (* (x p1) (x p2))
         (* (y p1) (y p2))))

(defmethod div ((p1 point) (p2 point))
  (point (/ (x p1) (x p2))
         (/ (y p1) (y p2))))

(defmethod neg ((p point))
  (point (- (x p))
         (- (y p))))

(defmethod midpoint ((p1 point) (p2 point))
  (div (add p1 p2) (spoint 2)))

(defvar *origin* (point 0 0))
(defvar *unit-point* (point 1 1))

(defclass rect (geometric)
  ((minpoint
    :initarg :minpoint
    :reader minpoint
    :reader point-designator)
   (maxpoint
    :initarg :maxpoint
    :reader maxpoint)))

(defmethod eqv ((r1 rect) (r2 rect))
  (and (eqv (minpoint r1) (minpoint r2))
       (eqv (maxpoint r1) (maxpoint r2))))

(defmethod width ((rect rect))
  (- (x (maxpoint rect))
     (x (minpoint rect))))

(defmethod height ((rect rect))
  (- (y (maxpoint rect))
     (y (minpoint rect))))

(defmethod pretty-components ((rect rect))
  (format nil "~Ax~A at ~A,~A"
          (width rect)
          (height rect)
          (x rect)
          (y rect)))

(defgeneric rect (xmin ymin xmax ymax)
  (:method (xmin ymin xmax ymax)
    (make-instance 'rect
                   :minpoint (point xmin ymin)
                   :maxpoint (point xmax ymax))))

(defmethod combine ((p1 point) (p2 point))
  (let ((x1 (x p1))
        (y1 (y p1))
        (x2 (x p2))
        (y2 (y p2)))
    (rect (min x1 x1) (max x1 x2)
          (min y1 y2) (max y1 y2))))

(defmethod initialize-instance :before ((rect rect) &key minpoint maxpoint
                                                      &allow-other-keys)
  (unless (and (<= (x minpoint) (x maxpoint))
               (<= (y minpoint) (y maxpoint)))
    (geometry-error "Minpoint and maxpoint are not oriented properly")))

(defgeneric point-rect (minpoint maxpoint)
  (:method (minpoint maxpoint)
    (make-instance 'rect
                   :minpoint minpoint
                   :maxpoint maxpoint)))

(defmethod xmin ((rect rect))
  (x (minpoint rect)))

(defmethod ymin ((rect rect))
  (y (minpoint rect)))

(defmethod xmax ((rect rect))
  (x (maxpoint rect)))

(defmethod ymax ((rect rect))
  (y (maxpoint rect)))

(defmethod expand ((rect rect) amount)
  (rect (- (xmin rect) amount)
        (- (ymin rect) amount)
        (+ (xmax rect) amount)
        (+ (ymax rect) amount)))

