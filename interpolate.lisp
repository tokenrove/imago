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

(defgeneric interpolate-pixel (image x y method)
  (:documentation "Get pixel from the image at point (X, Y) according
to some interpolation METHOD. X and Y are single floats in the range
[0, 1]."))

;;; Nearest neighbor

(defmethod interpolate-pixel ((image image) x y (method (eql :nearest-neighbor)))
  (declare (type (single-float 0.0 1.0) x y)
           (optimize (speed 3)))
  (with-image-definition (image width height pixels)
    (declare (type alex:positive-fixnum width height)
             (type (simple-array * (* *)) pixels))
    (let ((x% (floor (* x width)))
          (y% (floor (* y height))))
      (aref pixels y% x%))))

;;; Linear interpolation

(sera:-> linear-interpolation
         (single-float single-float single-float)
         (values single-float &optional))

(defun linear-interpolation (v1 v2 x)
  (declare (optimize (speed 3))
           (type single-float v1 v2 x))
  (+ v1 (* (- v2 v1) x)))

(defun interpolate-pixel-linear (image x y constructor accessors)
  (declare (type (single-float 0.0 1.0) x y)
           (type function constructor)
           (optimize (speed 3)))
  (with-image-definition (image width height pixels)
    (declare (type alex:positive-fixnum width height)
             (type (simple-array * (* *)) pixels))
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
                   (mapcar #'interpolate accessors))))))))

(defmethod interpolate-pixel ((image grayscale-image) x y (method (eql :bilinear)))
  (interpolate-pixel-linear image x y #'make-gray
                            (list #'gray-intensity #'gray-alpha)))

(defmethod interpolate-pixel ((image rgb-image) x y (method (eql :bilinear)))
  (interpolate-pixel-linear image x y #'make-color
                            (list #'color-red #'color-green
                                  #'color-blue #'color-alpha)))

;;; Bicubic interpolation

(alex:define-constant +resize-offsets+
    #((-1 . -1) (-1 . 0) (-1 . 1) (-1 . 2)
      (0  . -1) (0  . 0) (0  . 1) (0  . 2)
      (1  . -1) (1  . 0) (1  . 1) (1  . 2)
      (2  . -1) (2  . 0) (2  . 1) (2  . 2))
  :test #'equalp)

(declaim (inline interpolate-pixel-cubic))

(defun interpolate-pixel-cubic (image x y constructor accessors)
  (declare (optimize speed))
  (declare (type (single-float 0f0 1f0) x y))
  (declare (type function constructor))
  (locally (declare (optimize (debug 0)))
    (with-image-definition (image width height image-pixels)
      (declare (type (unsigned-byte 32) height width))
      (declare (type (simple-array (unsigned-byte 32) (* *)) image-pixels))
      (let ((x (- (* x width) 0.5f0))
            (y (- (* y height) 0.5f0)))
        (declare (type (single-float -10f10 10f10) x y))
        (labels ((compute-neighborhood (real-y real-x neighborhood)
                   (dotimes (i (length +resize-offsets+))
                     (let* ((cell (aref +resize-offsets+ i))
                            (offset-y (car cell))
                            (offset-x (cdr cell)))
                       (declare (type (signed-byte 8) offset-x offset-y))
                       (let ((y (ldb (byte 32 0) (+ real-y offset-y)))
                             (x (ldb (byte 32 0) (+ real-x offset-x))))
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
                   (declare (type single-float x y))
                   (declare (type (simple-array (unsigned-byte 32) (16)) pixels))
                   (declare (type function accessor))
                   (macrolet ((px (n) `(float (the (unsigned-byte 32)
                                                   (funcall accessor (aref pixels ,n)))
                                              1f0)))
                     (let ((x1 (cubic y (px 0) (px 1) (px 2) (px 3)))
                           (x2 (cubic y (px 4) (px 5) (px 6) (px 7)))
                           (x3 (cubic y (px 8) (px 9) (px 10) (px 11)))
                           (x4 (cubic y (px 12) (px 13) (px 14) (px 15))))
                       (the (single-float -10f10 10f10) (cubic x x1 x2 x3 x4))))))
          (declare (inline compute-neighborhood cubic bicubic))
          (multiple-value-bind (real-x interp-x) (truncate x)
            (multiple-value-bind (real-y interp-y) (truncate y)
              (let ((neighborhood (make-array 16 :element-type '(unsigned-byte 32))))
                (declare (dynamic-extent neighborhood))
                (compute-neighborhood real-y real-x neighborhood)
                (flet ((access (accessor)
                         (let* ((interpolated (bicubic interp-y interp-x neighborhood accessor))
                                (clamped (clamp (round interpolated) 0 255)))
                           clamped)))
                  (apply constructor (mapcar #'access accessors)))))))))))

(defmethod interpolate-pixel ((image rgb-image) x y (method (eql :bicubic)))
  (interpolate-pixel-cubic image x y #'make-color
                           (list #'color-red #'color-green
                                 #'color-blue #'color-alpha)))

(defmethod interpolate-pixel ((image grayscale-image) x y (method (eql :bicubic)))
  (interpolate-pixel-cubic image x y #'make-gray
                           (list #'gray-intensity #'gray-alpha)))
