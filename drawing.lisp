;;; IMAGO library
;;; Drawing simple shapes
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
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
  "Draws a line between two points in an image."
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

(defun draw-rectangle (image x1 y1 width height color
                       &key (dash-length 1) (dash-interval 0))
  "Draws a rectangle in an image."
  (let ((x2 (+ x1 width -1))
        (y2 (+ y1 height -1)))
    (draw-line image x1 y1 x2 y1 color
               :dash-length dash-length :dash-interval dash-interval)
    (draw-line image x1 y2 x2 y2 color
               :dash-length dash-length :dash-interval dash-interval)
    (draw-line image x1 y1 x1 y2 color
               :dash-length dash-length :dash-interval dash-interval)
    (draw-line image x2 y1 x2 y2 color
               :dash-length dash-length :dash-interval dash-interval)))

(defun draw-polygon (image coord-list color
                     &key (closed t) (dash-length 1) (dash-interval 0))
  "Draws a polygon in an image."
  (loop for (x1 y1 x2 y2) on coord-list by #'cddr
        do (when (and closed (null x2) (null y2))
             (setf x2 (first coord-list)
                   y2 (second coord-list)))
           (unless (or (null x2) (null y2))
             (draw-line image x1 y1 x2 y2 color
                        :dash-length dash-length
                        :dash-interval dash-interval))))

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
