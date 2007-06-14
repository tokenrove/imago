;;; IMAGO library
;;; Image filters (based on 5x5 convolution matrices)
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defgeneric convolve (image matrix divisor offset)
  (:documentation "Applies a 5x5 convolution kernel (a 5x5 real number
matrix) to an image. Returns the resulting image."))

(defmethod convolve ((image rgb-image) matrix divisor offset)
  (with-image-definition (image width height pixels)
    (declare (ignore pixels))
    (let ((image2 (make-instance 'rgb-image
                                 :width width :height height)))
      (do-image-pixels (image2 pixel2 x y)
        (let ((sum-r 0)
              (sum-g 0)
              (sum-b 0))
          (loop for dy from -2 to 2
                as y2 = (+ y dy)
                when (<= 0 y2 (1- height))
                do (loop for dx from -2 to 2
                         as x2 = (+ x dx)
                         when (<= 0 x2 (1- width))
                         do (let ((factor (aref matrix (+ dy 2) (+ dx 2)))
                                  (color (image-pixel image x2 y2)))
                              (multiple-value-bind (r g b)
                                  (color-rgb color)
                                (incf sum-r (* r factor))
                                (incf sum-g (* g factor))
                                (incf sum-b (* b factor))))))
          (let ((r (+ (floor sum-r divisor) offset))
                (g (+ (floor sum-g divisor) offset))
                (b (+ (floor sum-b divisor) offset)))
            (setf pixel2 (make-color (limit-value r 0 255)
                                     (limit-value g 0 255)
                                     (limit-value b 0 255))))))
      image2)))

(defmethod convolve ((image grayscale-image) matrix divisor offset)
  (with-image-definition (image width height pixels)
    (declare (ignore pixels))
    (let ((image2 (make-instance 'grayscale-image
                                 :width width :height height)))
      (do-image-pixels (image2 pixel2 x y)
        (let ((sum 0))
          (loop for dy from -2 to 2
                as y2 = (+ y dy)
                when (<= 0 y2 (1- height))
                do (loop for dx from -2 to 2
                         as x2 = (+ x dx)
                         when (<= 0 x2 (1- width))
                         do (let ((factor (aref matrix (+ dy 2) (+ dx 2))))
                              (incf sum (* (gray-intensity
                                            (image-pixel image x2 y2))
                                           factor)))))
          (let ((color (+ (floor sum divisor) offset)))
            (setf pixel2 (make-gray (limit-value color 0 255))))))
      image2)))

(defun blur (image)
  (convolve image #2A(( 0  0  0  0  0)
                      ( 0  1  1  1  0)
                      ( 0  1  1  1  0)
                      ( 0  1  1  1  0)
                      ( 0  0  0  0  0)) 9 0))

(defun sharpen (image)
  (convolve image #2A(( 0  0  0  0  0)
                      ( 0  0 -1  0  0)
                      ( 0 -1  5 -1  0)
                      ( 0  0 -1  0  0)
                      ( 0  0  0  0  0)) 1 0))

(defun edge-detect (image)
  (convolve image #2A(( 0  0  0  0  0)
                      ( 0  0  1  0  0)
                      ( 0  1 -4  1  0)
                      ( 0  0  1  0  0)
                      ( 0  0  0  0  0)) 1 0))

(defparameter +emboss-matrix-cell-angles+
  #2A((#.(* pi 0.75) #.(* pi 0.50) #.(* pi 0.25))
      (#.(* pi 1.00)           nil #.(* pi 0.00))
      (#.(* pi 1.25) #.(* pi 1.50) #.(* pi 1.75))))

(defun emboss (image &key (angle 0) (depth 1))
  (let ((matrix (make-array '(5 5) :initial-element 0)))
    (dotimes (y 3)
      (dotimes (x 3)
        (unless (and (= x 1) (= y 1))
          (let ((cell-angle (aref +emboss-matrix-cell-angles+ y x)))
            (setf (aref matrix (1+ y) (1+ x))
                  (* (cos (- cell-angle angle)) depth))))))
    (convolve image matrix 1 128)))

