;;; IMAGO library
;;; Image composition
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defgeneric compose (dest image1 image2 x y operator)
  (:documentation "Composes IMAGE1 and IMAGE2 at offset (X, Y), using
OPERATOR to compose each pixel. OPERATOR must be a function of two colors,
returning a color."))

(defgeneric default-compose-operator (image)
  (:documentation "Returns a compose operator that can be applied to
images of the same type as IMAGE. The default operator mixes colors
according to their respective alpha component."))


(defmethod compose ((dest (eql nil)) (image1 image) (image2 image)
                    x y operator)
  (let ((dest (make-similar-image image1)))
    (compose dest image1 image2 x y operator)))

(defmethod compose ((dest image) (image1 image) (image2 image)
                    x y operator)
  (assert (and (images-same-type-p dest image1 image2)
               (images-same-dimensions-p dest image1 dest)))
  (unless (eq image1 dest)
    (copy dest image1))
  (unless (in-image-p x y image1)
    (return-from compose dest))
  (with-image-definition (image1 width1 height1 pixels1)
    (let ((width (min (image-width image2) (- width1 x)))
          (height (min (image-height image2) (- height1 y))))
      (loop for y1 from y
            for y2 from 0
            repeat height
            do (loop for x1 from x
                     for x2 from 0
                     repeat width
                     do (setf (image-pixel dest x1 y1)
                              (funcall operator
                                       (image-pixel image1 x1 y1)
                                       (image-pixel image2 x2 y2)))))))
  dest)


(defmethod default-compose-operator ((image rgb-image))
  (lambda (color1 color2)
    (multiple-value-bind (a1 r1 g1 b1) (color-argb color1)
      (multiple-value-bind (a2 r2 g2 b2) (color-argb color2)
        (let* ((coef1 (if (>= a1 a2) (- a1 a2) 0))
               (divisor (if (>= a1 a2) a1 a2))
               (r (floor (+ (* coef1 r1) (* a2 r2)) divisor))
               (g (floor (+ (* coef1 g1) (* a2 g2)) divisor))
               (b (floor (+ (* coef1 b1) (* a2 b2)) divisor)))
          (make-color r g b a1))))))
