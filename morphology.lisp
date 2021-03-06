;;; IMAGO library
;;; Morphology functions
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
  (declare (type fixnum x min max)
           (optimize (speed 3)))
  (max (min x max) min))

(declaim (inline add-indices)
         (ftype (function (list list list)
                          (values list &optional))
                add-indices))
(defun add-indices (x y dimensions)
  (declare (optimize (speed 3)))
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
           (type list connectivity)
           (optimize (speed 3)))
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
                  while (not (null queue)) do
                    (let ((index (pop queue)))
                      (map 'nil
                           (lambda (shift)
                             (apply #'push-in-queue
                                    (add-indices index shift dimensions)))
                           connectivity))
                  finally (incf current-label delta)))))
      output)))

(defun component-boxes (components)
  "Return bounding boxes ((XMIN YMIN) (XMAX YMAX)) for connected
components of an image. COMPONENTS is an array returned by LABEL-COMPONENTS"
  (declare (type (simple-array fixnum (* *)) components))
  (let ((initial-box '((#.most-positive-fixnum
                        #.most-positive-fixnum)
                       (0 0)))
        (boxes (make-hash-table)))
    (array-operations/utilities:nested-loop (y x)
        (array-dimensions components)
      (let ((element (aref components y x)))
        (destructuring-bind (min max)
            (gethash element boxes initial-box)
          (setf (gethash element boxes)
                (list
                 (mapcar #'min min (list x y))
                 (mapcar #'max max (list x y)))))))
    (loop for component fixnum from 1 by 1
          for box = (gethash component boxes)
          while box collect box)))

;; Stolen from convolve.lisp ;)
(defun erode (image)
  "Erode binary image with 3x3 square structuring component."
  (declare (type binary-image image))
  (with-image-definition (image width height pixels)
    (declare (ignore pixels))
    (let ((image2 (make-instance 'binary-image
                                 :width  width
                                 :height height)))
      (do-image-pixels (image2 pixel2 x y)
        (setf pixel2
              (loop for dy from -1 to 1
                    as y2 = (+ y dy)
                    when (<= 0 y2 (1- height))
                      minimize
                      (loop for dx from -1 to 1
                            as x2 = (+ x dx)
                            when (<= 0 x2 (1- width))
                              minimize (image-pixel image x2 y2)))))
      image2)))

(defun dilate (image)
  "Dilate binary image with 3x3 square structuring component."
  (declare (type binary-image image))
  (with-image-definition (image width height pixels)
    (declare (ignore pixels))
    (let ((image2 (make-instance 'binary-image
                                 :width  width
                                 :height height)))
      (do-image-pixels (image2 pixel2 x y)
        (setf pixel2
              (loop for dy from -1 to 1
                    as y2 = (+ y dy)
                    when (<= 0 y2 (1- height))
                      maximize
                      (loop for dx from -1 to 1
                            as x2 = (+ x dx)
                            when (<= 0 x2 (1- width))
                              maximize (image-pixel image x2 y2)))))
      image2)))

(declaim
 (ftype
  (function ((array fixnum (*)))
            (values (array fixnum (*)) &optional))
  do-mdt-pass))
(defun do-mdt-pass (array)
  (declare (type (array fixnum (*)) array))
  (let ((length (length array)))
    (loop for i from 1 below length do
      (setf (aref array i)
            (min (1+ (aref array (1- i)))
                 (aref array i))))
    (loop for i from (- length 2) downto 0 do
      (setf (aref array i)
            (min (1+ (aref array (1+ i)))
                 (aref array i))))
    array))

(defun manhattan-distance-transform (image)
  "Perform Manhattan distance transform on a binary image."
  (declare (type binary-image image))
  (with-image-definition (image width height pixels)
    (let ((distances (make-array (list height width)
                                 :element-type 'fixnum)))
      ;; Initialize the array with distances
      (map-into (aops:flatten distances)
                (lambda (x) (* (- 1 x) most-positive-fixnum))
                (aops:flatten pixels))
      ;; Walk through the rows of the array and calculate MDT for each
      ;; row separately.
      (dotimes (row height)
        (do-mdt-pass (make-array width
                                 :element-type 'fixnum
                                 :displaced-to distances
                                 :displaced-index-offset (* row width))))
      ;; Now walk through the columns. Have to permute the array for that :(
      (let ((permutation (aops:permute '(1 0) distances)))
        (dotimes (column width)
          (do-mdt-pass (make-array height
                                   :element-type 'fixnum
                                   :displaced-to permutation
                                   :displaced-index-offset (* column height))))
        (aops:permute '(1 0) permutation)))))
