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


(defparameter *default-interpolation* :nearest-neighbor
  "Default interpolation method. See RESIZE for details.")

(defgeneric interpolator (image method)
  (:documentation "Return a function which accepts to single float
values X and Y in the range [0, 1) and returns a pixel at those
coordinates"))

;;; Nearest neighbor

(defmethod interpolator ((image image) (method (eql :nearest-neighbor)))
  (declare (optimize (speed 3)))
  (with-image-definition (image width height pixels)
    (declare (type alex:positive-fixnum width height)
             (type (simple-array * (* *)) pixels))
    (lambda (x y)
      (declare (type (single-float 0f0 1f0) x y))
      (let ((x% (floor (* x width)))
            (y% (floor (* y height))))
        (aref pixels y% x%)))))

;;; Linear interpolation

(sera:-> linear-interpolation
         (single-float single-float single-float)
         (values single-float &optional))

(defun linear-interpolation (v1 v2 x)
  (declare (optimize (speed 3))
           (type single-float v1 v2 x))
  (+ v1 (* (- v2 v1) x)))

;; Correct return type in polymorphs. Now it's just FUNCTION, probably
;; due to bug in polymorphic-functions.
#+nil
(sera:-> ((single-float 0f0 1f0)
          (single-float 0f0 1f0))
         (values <t> &optional))

(defpolymorph linear-interpolator ((pixels (array <t>))
                                   (constructor function)
                                   (accessors list))
    function
  (destructuring-bind (height width)
      (array-dimensions pixels)
    (declare (type alex:positive-fixnum width height))
    (lambda (x y)
      (declare (type (single-float 0f0 1f0) x y))
      (multiple-value-bind (x rem-x) (floor (* x width))
        (multiple-value-bind (y rem-y) (floor (* y height))
          (let* ((x+1 (min (1- width)  (1+ x)))
                 (y+1 (min (1- height) (1+ y)))
                 (pixel-x0-y0 (aref pixels y   x))
                 (pixel-x1-y0 (aref pixels y   x+1))
                 (pixel-x0-y1 (aref pixels y+1 x))
                 (pixel-x1-y1 (aref pixels y+1 x+1)))
            (flet ((interpolate (accessor)
                     (declare (type (sera:-> ((unsigned-byte 32))
                                             (values (unsigned-byte 8) &optional))
                                    accessor))
                     (let ((v1 (linear-interpolation
                                (float (funcall accessor pixel-x0-y0))
                                (float (funcall accessor pixel-x1-y0))
                                rem-x))
                           (v2 (linear-interpolation
                                (float (funcall accessor pixel-x0-y1))
                                (float (funcall accessor pixel-x1-y1))
                                rem-x)))
                       (floor (linear-interpolation v1 v2 rem-y)))))
              (apply constructor
                     (mapcar #'interpolate accessors)))))))))

(defmethod interpolator ((image rgb-image) (method (eql :bilinear)))
  (declare (optimize (speed 3)))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array rgb-pixel) pixels))
    (linear-interpolator
     pixels
     #'make-color
     (list #'color-red #'color-green
           #'color-blue #'color-alpha))))

(defmethod interpolator ((image grayscale-image) (method (eql :bilinear)))
  (declare (optimize (speed 3)))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array grayscale-pixel) pixels))
    (linear-interpolator
     pixels
     #'make-gray
     (list #'gray-intensity #'gray-alpha))))

;;; Bicubic interpolation

(alex:define-constant +resize-offsets+
    #((-1 . -1) (-1 . 0) (-1 . 1) (-1 . 2)
      (0  . -1) (0  . 0) (0  . 1) (0  . 2)
      (1  . -1) (1  . 0) (1  . 1) (1  . 2)
      (2  . -1) (2  . 0) (2  . 1) (2  . 2))
  :test #'equalp)

(defpolymorph cubic-interpolator ((image-pixels (array <t>))
                                  (constructor function)
                                  (accessors list))
    function
  (destructuring-bind (height width)
      (array-dimensions image-pixels)
    (declare (type alex:positive-fixnum height width))
    (lambda (x y)
      (declare (type (single-float 0f0 1f0) x y))
      (let ((x (- (* x width) 0.5f0))
            (y (- (* y height) 0.5f0)))
        (labels ((compute-neighborhood (real-y real-x neighborhood)
                   (dotimes (i (length +resize-offsets+))
                     (let* ((cell (aref +resize-offsets+ i))
                            (offset-y (car cell))
                            (offset-x (cdr cell)))
                       (declare (type (signed-byte 8) offset-x offset-y))
                       (let ((y (+ real-y offset-y))
                             (x (+ real-x offset-x)))
                         (setf (aref neighborhood i)
                               (aref image-pixels
                                     (clamp y 0 (1- height))
                                     (clamp x 0 (1- width))))))))
                 (cubic (x p0 p1 p2 p3)
                   (declare (type single-float x p0 p1 p2 p3))
                   (let* ((step1 (* x (+ (* 3f0 (- p1 p2)) p3 (- p0))))
                          (step2 (* x (+ (* 2f0 p0) (* -5f0 p1)
                                         (* 4f0 p2) (- p3) step1)))
                          (step3 (* x (+ p2 (- p0) step2))))
                     (+ p1 (* 0.5f0 step3))))
                 (bicubic (x y pixels accessor)
                   (declare (type single-float x y)
                            (type function accessor))
                   (macrolet ((px (n) `(float (the (unsigned-byte 8)
                                                   (funcall accessor (aref pixels ,n)))
                                              1f0)))
                     (let ((x1 (cubic y (px 0) (px 1) (px 2) (px 3)))
                           (x2 (cubic y (px 4) (px 5) (px 6) (px 7)))
                           (x3 (cubic y (px 8) (px 9) (px 10) (px 11)))
                           (x4 (cubic y (px 12) (px 13) (px 14) (px 15))))
                       (cubic x x1 x2 x3 x4)))))
          (declare (inline compute-neighborhood cubic bicubic))
          (multiple-value-bind (real-x interp-x) (truncate x)
            (multiple-value-bind (real-y interp-y) (truncate y)
              (let ((neighborhood (make-array 16 :element-type <t>)))
                (declare (dynamic-extent neighborhood))
                (compute-neighborhood real-y real-x neighborhood)
                (flet ((access (accessor)
                         (let* ((interpolated (bicubic interp-y interp-x neighborhood accessor))
                                (clamped (clamp (round interpolated) 0 255)))
                           clamped)))
                  (apply constructor (mapcar #'access accessors)))))))))))

(defmethod interpolator ((image rgb-image) (method (eql :bicubic)))
  (declare (optimize (speed 3)))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array rgb-pixel) pixels))
    (cubic-interpolator
     pixels #'make-color
     (list #'color-red #'color-green
           #'color-blue #'color-alpha))))

(defmethod interpolator ((image grayscale-image) (method (eql :bicubic)))
  (declare (optimize (speed 3)))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array grayscale-pixel) pixels))
    (cubic-interpolator
     pixels 
     #'make-gray
     (list #'gray-intensity #'gray-alpha))))
