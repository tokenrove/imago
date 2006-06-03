;;; IMAGO library
;;; Image related utilities
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(declaim (inline in-image-p))
(defun in-image-p (x y image)
  "Checks that the point with coordinates (X, Y) is inside IMAGE."
  (and (>= x 0)
       (< x (image-width image))
       (>= y 0)
       (< y (image-height image))))

(declaim (inline in-region-p))
(defun in-region-p (x y region-x region-y region-width region-height)
  "Checks that the point with coordinates (X, Y) is inside the given region."
  (and (>= x region-x)
       (< x (+ region-x region-width))
       (>= y region-y)
       (< y (+ region-y region-height))))

(defgeneric make-similar-image (image)
  (:documentation "Returns a new image, with same type and dimensions
as IMAGE."))

(defmethod make-similar-image ((image image))
  (make-instance (class-of image)
                 :width (image-width image)
                 :height (image-height image)))

(defun images-same-type-p (&rest images)
  (if (endp images)
      t
      (let ((class (class-of (first images))))
        (every (lambda (image)
                 (eq (class-of image) class))
               (rest images)))))

(defun images-same-dimensions-p (&rest images)
  (if (endp images)
      t
      (let ((width (image-width (first images)))
            (height (image-height (first images))))
        (every (lambda (image)
                 (and (= (image-width image) width)
                      (= (image-height image) height)))
               (rest images)))))

(defmacro with-image-definition ((image width height pixels) &body body)
  "Binds the WIDTH, HEIGHT and PIXELS variables to the values
corresponding to IMAGE, then evaluates the body."
  `(let ((,width (image-width ,image))
         (,height (image-height ,image))
         (,pixels (image-pixels ,image)))
    ,@body))

(defmacro do-image-pixels ((image color x y) &body body)
  "Iterates through all pixels of an image. For each pixel,
X and Y are bound to the pixel coordinates, and COLOR is a
generalized variable corresponding to the pixel color."
  `(do-region-pixels (,image ,color ,x ,y
                      0 0 (image-width ,image) (image-height ,image))
    ,@body))

(defmacro do-region-pixels ((image color x y
                             region-x region-y region-width region-height)
                            &body body)
  "Iterates through all pixels of a rectangular region in an image.
For each pixel, X and Y are bound to the pixel coordinates, and COLOR is a
generalized variable corresponding to the pixel color."
  (with-gensyms (image-width pixels x1 y1 width height
                 index line-jump xrel yrel)
    `(let* ((,image-width (image-width ,image))
            (,pixels (image-pixels ,image))
            (,x1 ,region-x)
            (,y1 ,region-y)
            (,width ,region-width)
            (,height ,region-height)
            (,index (+ (* ,y1 ,image-width) ,x1))
            (,line-jump (- ,image-width ,width))
            (,x ,x1)
            (,y ,y1))
      (symbol-macrolet ((,color (row-major-aref ,pixels ,index)))
        (dotimes (,yrel ,height)
          (dotimes (,xrel ,width)
            ,@body
            (incf ,index)
            (incf ,x))
          (incf ,index ,line-jump)
          (incf ,y)
          (setf ,x ,x1))))))

(defmacro do-line-pixels ((image color x y x1 y1 x2 y2) &body body)
  "Iterates through all pixels of a line segment in an image,
corresponding to the X1, Y1, X2 and Y2 parameters. For each pixel,
X and Y are bound to the pixel coordinates, and COLOR is a
generalized variable corresponding to the pixel color."
  (with-gensyms (width pixels dx dy adx ady index count
                 x-inc y-inc index-y-inc errmax errinc err)
    `(let* ((,width (image-width ,image))
            (,pixels (image-pixels ,image))
            (,x ,x1)
            (,y ,y1)
            (,dx (- ,x2 ,x))
            (,dy (- ,y2 ,y))
            (,adx (abs ,dx))
            (,ady (abs ,dy))
            (,x-inc (if (plusp ,dx) 1 -1))
            (,y-inc (if (plusp ,dy) 1 -1))
            (,index-y-inc (if (plusp ,dy) ,width (- ,width)))
            (,index (+ (* ,width ,y) ,x)))
      (symbol-macrolet ((,color (row-major-aref ,pixels ,index)))
        (if (>= ,adx ,ady)
            (let* ((,count (1+ ,adx))
                   (,errmax ,adx)
                   (,errinc ,ady)
                   (,err (floor ,errmax 2)))
              (loop repeat ,count
                    do ,@body
                    (incf ,err ,errinc)
                    (when (>= ,err ,errmax)
                      (incf ,y ,y-inc)
                      (incf ,index ,index-y-inc)
                      (decf ,err ,errmax))
                    (incf ,index ,x-inc)
                    (incf ,x ,x-inc)))
            (let* ((,count (1+ ,ady))
                   (,errmax ,ady)
                   (,errinc ,adx)
                   (,err (floor ,errmax 2)))
              (loop repeat ,count
                    do ,@body
                    (incf ,err ,errinc)
                    (when (>= ,err ,errmax)
                      (incf ,x ,x-inc)
                      (incf ,index ,x-inc)
                      (decf ,err ,errmax))
                    (incf ,index ,index-y-inc)
                    (incf ,y ,y-inc))))))))

(defgeneric set-alpha (image alpha)
  (:documentation "Sets the alpha channel value for all pixels/colors
in an image."))

(defmethod set-alpha ((image rgb-image) alpha)
  (do-image-pixels (image color x y)
    (multiple-value-bind (r g b)
        (color-rgb color)
      (setf color (make-color r g b alpha)))))
  
(defmethod set-alpha ((image grayscale-image) alpha)
  (do-image-pixels (image color x y)
    (let ((intensity (gray-intensity color)))
      (setf color (make-gray intensity alpha)))))
  
(defmethod set-alpha ((image indexed-image) alpha)
  (with-slots (colormap) image
    (loop for i below (length colormap)
          do (multiple-value-bind (r g b)
                 (color-rgb (aref colormap i))
               (setf (aref colormap i) (make-color r g b alpha))))))
