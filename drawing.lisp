;;; IMAGO library
;;; Drawing simple shapes
;;;
;;; Copyright (C) 2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

(defun draw-point (image x y color)
  "Draws a point in an image."
  (when (in-image-p x y image)
    (setf (image-pixel image x y) color)))

(defun draw-line (image x1 y1 x2 y2 color
                  &key (dash-length 1) (dash-interval 0))
  (let ((drawing t)
        (counter 0))
    (do-line-pixels (image pixel x y x1 y1 x2 y2)
      (when (and drawing (in-image-p x y image))
        (setf pixel color))
      (incf counter)
      (if drawing
          (when (and (plusp dash-interval) (>= counter dash-length))
            (setf drawing nil
                  counter 0))
          (when (and (plusp dash-length) (>= counter dash-interval))
            (setf drawing t
                  counter 0))))))

(defun draw-rectangle (image x1 y1 width height color)
  "Draws a rectangle in an image."
  (let* ((image-width (image-width image))
         (pixels (image-pixels image))
         (index (+ (* y1 image-width) x1)))
    (loop for index2 = index then (1+ index2)
          repeat width
          do (setf (row-major-aref pixels index2) color))
    (loop for index2 = (+ index (* (1- height) image-width)) then (1+ index2)
          repeat width
          do (setf (row-major-aref pixels index2) color))
    (loop for index2 = (+ index image-width) then (+ index2 image-width)
          repeat (- height 2)
          do (setf (row-major-aref pixels index2) color))
    (loop for index2 = (+ index image-width width -1)
          then (+ index2 image-width)
          repeat (- height 2)
          do (setf (row-major-aref pixels index2) color))))

(defun draw-circle (image center-x center-y radius color)
  "Draws a circle in an image."
  (flet ((circle-points (x y color)
           (draw-point image (+ center-x x) (+ center-y y) color)
           (draw-point image (+ center-x y) (+ center-y x) color)
           (draw-point image (+ center-x y) (- center-y x) color)
           (draw-point image (+ center-x x) (- center-y y) color)
           (draw-point image (- center-x x) (- center-y y) color)
           (draw-point image (- center-x y) (- center-y x) color)
           (draw-point image (- center-x y) (+ center-y x) color)
           (draw-point image (- center-x x) (+ center-y y) color)))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (circle-points x y color)
      (loop while (> y x)
            do (cond ((minusp d)
                      (incf d delta-e)
                      (incf delta-e 2)
                      (incf delta-se 2))
                     (t
                      (incf d delta-se)
                      (incf delta-e 2)
                      (incf delta-se 4)
                      (decf y)))
               (incf x)
               (circle-points x y color)))))
