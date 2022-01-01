;;; IMAGO library
;;; Image operations
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

(defun rotate (image degree &key (interpolation *default-interpolation*)
                              (background-color (make-default-background-color image)))
  "Rotate an image by DEGREE degrees counterclockwise (negative DEGREE
gives clockwise rotation)."
  (let ((degree (mod degree 360))
        (dimensions (array-dimensions (image-pixels image))))
    (destructuring-bind (new-height new-width)
        (rotate-dimensions dimensions degree)
      (let ((pixels (make-array (list new-height new-width)
                                :element-type (array-element-type
                                               (image-pixels image))))
            (rotator (make-rotator image degree interpolation
                                   :background-color background-color)))
        (array-operations/utilities:nested-loop (y x) (array-dimensions pixels)
          (setf (aref pixels y x) (funcall rotator y x)))
        (make-instance (class-of image) :pixels pixels
                                        :width new-width :height new-height)))))

(defun make-rotator (image degree interpolation &key background-color)
  (declare (type (integer 0 360) degree))
  (labels ((center (dimensions)
             (map 'list (lambda (x) (/ (1- x) 2.0)) dimensions)))
    (let* ((src (image-pixels image))
           (src-dimensions (array-dimensions src))
           (dest-dimensions (rotate-dimensions src-dimensions (- degree)))
           (src-center (center src-dimensions))
           (dest-center (center dest-dimensions))
           (interpolator (interpolator image interpolation)))
      (declare (type function interpolator))
      (destructuring-bind ((src-height src-width)
                           (src-center-y src-center-x)
                           (dest-center-y dest-center-x))
          (list src-dimensions src-center dest-center)
        (declare (type alex:positive-fixnum src-height src-width)
                 (type single-float
                       src-center-y src-center-x
                       dest-center-y dest-center-x))
        (let* ((radians (* 2 (float pi 1f0) (- degree) 1/360))
               (cos (cos radians))
               (sin (sin radians)))
          (flet ((operate (dest-y dest-x)
                   (declare (optimize (speed 3))
                            (type alex:non-negative-fixnum dest-y dest-x))
                   (let* ((dest-y (float dest-y 0f0))
                          (dest-x (float dest-x 0f0))
                          (src-x (/ (+ (* sin (- dest-y dest-center-y))
                                       (* cos (- dest-x dest-center-x))
                                       src-center-x)
                                    (1- src-width)))
                          (src-y (/ (+ (* cos (- dest-y dest-center-y))
                                       (- (* sin (- dest-x dest-center-x)))
                                       src-center-y)
                                    (1- src-height))))
                     (declare (type single-float src-x src-y))
                     ;; FIXME: the background-color border is rough,
                     ;; interpolate it in the future. This will likely
                     ;; require passing the background color to
                     ;; INTERPOLATE-PIXEL though.
                     (if (and background-color
                              (or (<  src-x 0f0)
                                  (<= 1f0 src-x)
                                  (<  src-y 0f0)
                                  (<= 1f0 src-y)))
                         background-color
                         (funcall interpolator src-x src-y)))))
            #'operate))))))

(defgeneric make-default-background-color (image)
  (:method ((image rgb-image)) (make-color 0 0 0 0))
  (:method ((image grayscale-image)) (make-gray 0 0)))

(defun rotate-dimensions (dimensions degree)
  (case degree
    ((0 180) dimensions)
    ((90 270) (reverse dimensions))
    (t (destructuring-bind (height width) dimensions
         (let* ((radians (* 2 pi degree 1/360))
                (cos (abs (cos radians)))
                (sin (abs (sin radians)))
                (new-width (ceiling (+ (* width cos) (* height sin))))
                (new-height (ceiling (+ (* width sin) (* height cos)))))
           (list new-height new-width))))))
