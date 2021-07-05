;;; IMAGO library
;;; Connectivity labeling
;;;
;;; Copyright (C) 2021
;;; Matthieu Villeneuve (matthieu.villeneuve@free.fr), Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :imago)

(declaim (optimize (speed 3)))

(defparameter *cross-pattern*
  '((-1  0)
    ( 0 -1)
    ( 0  1)
    ( 1  0))
  "Neighborhood pattern for Manhattan distance. Two pixels are
considered neighbors if Manhattan distance between them is 1")

(defparameter *square-pattern*
  '((-1 -1)
    (-1  0)
    (-1  1)
    ( 0 -1)
    ( 0  1)
    ( 1 -1)
    ( 1  0)
    ( 1  1))
  "Neighborhood pattern for Chebyshev distance. Two pixels are
considered neighbors if Chebyshev distance between them is 1")

(declaim (inline clamp)
         (ftype (function (fixnum fixnum fixnum)
                          (values fixnum &optional))
                clamp))
(defun clamp (x min max)
  (declare (type fixnum x min max))
  (max (min x max) min))

(declaim (inline add-indices)
         (ftype (function (list list list)
                          (values list &optional))
                add-indices))
(defun add-indices (x y dimensions)
  (mapcar
   (lambda (x y max)
     (declare (type fixnum x y max))
     (clamp (+ x y) 0 (1- max)))
   x y dimensions))


(defun label-components (image &key (connectivity *cross-pattern*))
  "Perform connected components labeling on binary image. Pixels with
value zero are considered background. Each cluster gets a unique
integer label. The result is returned in an array of fixnums with the
same dimenions as image."
  (declare (type binary-image image)
           (type list connectivity))
  (with-image-definition (image width height pixels)
    (declare (type (simple-array bit (* *)) pixels))
    (let* ((dimensions (list height width))
           (output (make-array dimensions
                              :element-type 'fixnum
                              :initial-element -1))
          (current-label 1))
      (declare (type fixnum current-label))
      (do-image-pixels (image color x y)
        (let (queue)
          (declare (type list queue))
          (flet ((push-in-queue (y x)
                   (cond
                     ;; If an element is a background element, label it as so
                     ((zerop (aref pixels y x))
                      (setf (aref output y x) 0)
                      0)
                     ;; If the element does not have a label, assign the current label
                     ((= (aref output y x) -1)
                      (setf (aref output y x) current-label)
                      (push (list y x) queue)
                      1)
                     (t
                      ;; Already has a label - skip
                      0))))
            (loop with delta fixnum = (push-in-queue y x)
                  while (not (zerop (length queue))) do
                    (let ((index (pop queue)))
                      (map 'nil
                           (lambda (shift)
                             (apply #'push-in-queue
                                    (add-indices index shift dimensions)))
                           connectivity))
                  finally (incf current-label delta)))))
      output)))
