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

;; ====================
;; Component labeling
;; ====================
(alex:define-constant +cross-pattern+
  '((-1  0)
    ( 0 -1)
    ( 0  1)
    ( 1  0))
  :test #'equalp
  :documentation "Neighborhood pattern for Manhattan distance. Two
pixels are considered neighbors if Manhattan distance between them is
1")

(alex:define-constant +square-pattern+
  '((-1 -1)
    (-1  0)
    (-1  1)
    ( 0 -1)
    ( 0  1)
    ( 1 -1)
    ( 1  0)
    ( 1  1))
  :test #'equalp
  :documentation "Neighborhood pattern for Chebyshev distance. Two
pixels are considered neighbors if Chebyshev distance between them is
1")

(defparameter *structuring-element*
  (make-array '(3 3)
              :element-type 'bit
              :initial-contents '((1 1 1)
                                  (1 1 1)
                                  (1 1 1)))
  "Default structuring element for erosion and dilation")

(sera:-> add-indices (list list list) (values list &optional))
(declaim (inline add-indices))

(defun add-indices (x y dimensions)
  (declare (optimize (speed 3)))
  (mapcar
   (lambda (x y max)
     (declare (type fixnum x y max))
     (clamp (+ x y) 0 (1- max)))
   x y dimensions))


(defun label-components (image &key (connectivity +cross-pattern+))
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
                  until (null queue) do
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

;; ====================
;; Erode & Dilate
;; ====================
(sera:-> kernel-some
         ((simple-array bit (* *))
          (simple-array bit (* *))
          alex:non-negative-fixnum alex:non-negative-fixnum
          (sera:-> (bit bit) (values boolean &optional)))
         (values boolean &optional))
(declaim (inline kernel-some))
(defun kernel-some (kernel array i j fn)
  (let* ((kh (array-dimension kernel 0))
         (kw (array-dimension kernel 1))
         (kh/2 (floor kh 2))
         (kw/2 (floor kw 2)))
    (loop for k fixnum below kh do
          (loop for l fixnum below kw do
                (when (funcall fn
                               (aref kernel k l)
                               (aref array
                                     (mod (+ i kh/2 (- k)) (array-dimension array 0))
                                     (mod (+ j kw/2 (- l)) (array-dimension array 1))))
                  (return-from kernel-some t)))))
  nil)

(macrolet ((def-morphological-op (name op succ documentation)
             `(progn
                (sera:-> ,name (binary-image &optional (simple-array bit (* *)))
                         (values binary-image &optional))
                (defun ,name (image &optional (structuring-element *structuring-element*))
                  ,documentation
                  (declare (optimize (speed 3)))
                  (let* ((width  (image-width  image))
                         (height (image-height image))
                         (result (make-binary-image width height))
                         (image-pixels  (image-pixels image))
                         (result-pixels (image-pixels result)))
                    (declare (type (simple-array bit (* *)) image-pixels result-pixels))
                    (aops:each-index! result-pixels (i j)
                      (if (kernel-some structuring-element image-pixels i j
                                       (lambda (kernel pixel)
                                         (declare (type bit kernel pixel))
                                         (and (not (zerop kernel))
                                              (,op (zerop pixel)))))
                          ,succ (- (1- ,succ))))
                    result)))))
  (def-morphological-op erode identity 0
    "Erode binary image. STRUCTURING-ELEMENT is an optional 2D simple
array of bits which serves as a structuring element and defaults to
*STRUCTURING-ELEMENT*.")
  (def-morphological-op dilate not 1
    "Dilate binary image. STRUCTURING-ELEMENT is an optional 2D simple
array of bits which serves as a structuring element and defaults to
*STRUCTURING-ELEMENT*."))

;; ====================
;; Distance transform
;; ====================
(sera:-> mdt-pass!
         ((array single-float (*)))
         (values (array single-float (*)) &optional))
(defun mdt-pass! (array)
  (declare (type (array single-float (*)) array))
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

(sera:-> edt-pass!
         ((array single-float (*)))
         (values (array single-float (*)) &optional))
(defun edt-pass! (array)
  (declare (type (array single-float (*)) array))
  (let ((length (length array))
        (envelope-minima (list 0))
        envelope-crossing)
    (loop for i fixnum from 1 below length do
         (loop
            for current-minima fixnum =
              (car envelope-minima)
            for crossing single-float =
              (/ (- (+ (expt i 2)
                       (expt (aref array i) 2))
                    (+ (expt current-minima 2)
                       (expt (aref array current-minima) 2)))
                 (* 2.0 (- i current-minima)))
            while (and envelope-crossing
                       (<= crossing (car envelope-crossing)))
            do
              (pop envelope-crossing)
              (pop envelope-minima)
            finally
              (push i envelope-minima)
              (push crossing envelope-crossing)))
    (loop
       with dist              = (copy-seq array)
       with envelope-minima   = (reverse envelope-minima)
       with envelope-crossing = (reverse envelope-crossing)
       for i fixnum below length do
         (loop while (and envelope-crossing
                          (< (car envelope-crossing) i))
            do
              (pop envelope-crossing)
              (pop envelope-minima))
         (setf (aref array i)
               (+ (expt (- i (car envelope-minima)) 2)
                  (expt (aref dist (car envelope-minima)) 2)))))
  array)

(declaim (inline distance-transform-pass!))
(defun distance-transform-pass! (type)
  (declare (type (member :mdt :edt) type))
  (ecase type
    (:mdt #'mdt-pass!)
    (:edt #'edt-pass!)))

(sera:-> distance-transform
         (image &key (:type symbol))
         (values (simple-array single-float (* *)) &optional))
(defun distance-transform (image &key (type :edt))
  "Perform distance transform on a binary image. Every 1 is replaced
with 0f0 and every 0 is replaced with distance to the closest 1.

TYPE can be either :MDT (Manhattan distance transform) or :EDT
(squared Euclidean distance transform)."
  (declare (type binary-image image))
  (with-image-definition (image width height pixels)
    (let ((dt-pass! (distance-transform-pass! type))
          ;; Initialize the array with distances
          (distances
           (let ((max-dim (expt (max width height) 2)))
             (aops:vectorize* 'single-float (pixels)
               (* (- 1.0 pixels) max-dim)))))
      ;; Walk through the rows of the array and calculate MDT for each
      ;; row separately.
      (dotimes (row height)
        ;; MDT-PASS! as the first pass is common for all metrics
        (mdt-pass! (make-array width
                               :element-type 'single-float
                               :displaced-to distances
                               :displaced-index-offset (* row width))))
      ;; Now walk through the columns. Have to permute the array for that :(
      (let ((permutation (aops:permute '(1 0) distances)))
        (dotimes (column width)
          (funcall dt-pass!
                   (make-array height
                               :element-type 'single-float
                               :displaced-to permutation
                               :displaced-index-offset (* column height))))
        (aops:permute '(1 0) permutation)))))

;; =========
;; Thinning
;; =========
(sera:-> thinning-pass (binary-image boolean)
         (values binary-image &optional))
(defun thinning-pass (image odd-iteration-p)
  (declare (optimize (speed 3)))
  (with-image-definition (image width height pixels)
    (declare (type (simple-array bit (* *)) pixels))
    (let ((new-pixels (make-array (list height width) :element-type 'bit)))
      (do-image-pixels (image pixel x y)
        (unless (zerop (aref pixels y x))
          (let* ((p1 (aref pixels (mod (+ y -1) height) (mod (+ x -1) width)))
                 (p2 (aref pixels (mod (+ y -1) height) (mod (+ x  0) width)))
                 (p3 (aref pixels (mod (+ y -1) height) (mod (+ x +1) width)))
                 (p4 (aref pixels (mod (+ y  0) height) (mod (+ x +1) width)))
                 (p5 (aref pixels (mod (+ y +1) height) (mod (+ x +1) width)))
                 (p6 (aref pixels (mod (+ y +1) height) (mod (+ x  0) width)))
                 (p7 (aref pixels (mod (+ y +1) height) (mod (+ x -1) width)))
                 (p8 (aref pixels (mod (+ y  0) height) (mod (+ x -1) width)))

                 (c (+ (logand (- 1 p2) (logior p3 p4))
                       (logand (- 1 p4) (logior p5 p6))
                       (logand (- 1 p6) (logior p7 p8))
                       (logand (- 1 p8) (logior p1 p2))))
                 (n1 (+ (logior p1 p2) (logior p3 p4)
                        (logior p5 p6) (logior p7 p8)))
                 (n2 (+ (logior p2 p3) (logior p4 p5)
                        (logior p6 p7) (logior p8 p1)))
                 (n (min n1 n2))
                 (o (if odd-iteration-p
                        (logand p4 (logior (- 1 p5) p3 p2))
                        (logand p8 (logior (- 1 p1) p7 p6)))))
            (setf (aref new-pixels y x)
                  (if (and (zerop o) (= c 1) (<= 2 n 3)) 0 1)))))
      (make-binary-image-from-pixels new-pixels))))

(sera:-> thin (binary-image) (values binary-image &optional))
(defun thin (image)
  "Perform thinning (extracting topological skeleton) of binary image."
  (declare (optimize (speed 3)))
  (loop with current = image
        for iteration fixnum from 1 by 1
        for next = (thinning-pass current (oddp iteration))
        until (equalp (image-pixels current)
                      (image-pixels next))
        do (setq current next)
        finally (return current)))
