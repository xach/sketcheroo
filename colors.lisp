;;;; colors.lisp

(in-package #:sketcheroo)

(defclass color () ())

(defclass rgba-color (color)
  ((red
    :initarg :red
    :accessor red)
   (green
    :initarg :green
    :accessor green)
   (blue
    :initarg :blue
    :accessor blue)
   (alpha
    :initarg :alpha
    :accessor alpha)))

(defgeneric rgba-color (red green blue &optional alpha)
  (:method (red green blue &optional alpha)
    (unless alpha
      (setf alpha 1.0d0))
    (make-instance 'rgba-color
                   :red (coerce red 'double-float)
                   :green (coerce green 'double-float)
                   :blue (coerce blue 'double-float)
                   :alpha (coerce alpha 'double-float))))

(defmethod print-object ((color color) stream)
  (print-unreadable-object (color stream :type t)
    (format stream "~A,~A,~A,~A"
            (red color) (green color) (blue color) (alpha color))))

(defgeneric rgb-color (red green blue)
  (:method (red green blue)
    (rgba-color red green blue 1.0d0)))

;;; from kmrcl
(defun rgb->hsv (r g b)
  (let* ((min (min r g b))
         (max (max r g b))
         (delta (- max min))
         (v max)
         (s 0.0d0)
         (h 0.0d0))
    (when (plusp max)
      (setq s (/ delta max)))
    (when (plusp delta)
      (setq h (cond
               ((= max r)
                (nth-value 0 (/ (- g b) delta)))
               ((= max g)
                (nth-value 0 (+ 2 (/ (- b r) delta))))
               (t
                (nth-value 0 (+ 4 (/ (- r g) delta))))))
      (setq h (* 60 h))
      (when (minusp h)
        (incf h 360)))
    (values h s v)))

(defun hsv->rgb (h s v)
  (when (zerop s)
    (return-from hsv->rgb (values v v v)))

  (loop while (minusp h)
        do (incf h 360))
  (loop while (>= h 360)
        do (decf h 360))

  (let ((h-pos (/ h 60)))
    (multiple-value-bind (h-int h-frac) (truncate h-pos)
      (declare (fixnum h-int))
      (let ((p (* v (- 1 s)))
            (q (* v (- 1 (* s h-frac))))
            (t_ (* v (- 1 (* s (- 1 h-frac)))))
            r g b)

        (cond
         ((zerop h-int)
          (setf r v
                g t_
                b p))
         ((= 1 h-int)
          (setf r q
                g v
                b p))
         ((= 2 h-int)
          (setf r p
                g v
                b t_))
         ((= 3 h-int)
          (setf r p
                g q
                b v))
         ((= 4 h-int)
          (setf r t_
                g p
                b v))
         ((= 5 h-int)
          (setf r v
                g p
                b q)))
        (values r g b)))))

(defun hsv-color (h s v)
  (multiple-value-call 'rgb-color (hsv->rgb h s v)))

(defgeneric hsv-values (color)
  (:method ((color color))
    (rgb->hsv (red color) (green color) (blue color))))

(defgeneric rgba-values (color)
  (:method ((color color))
    (values (red color) (green color) (blue color) (alpha color))))

