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
(deftype dt-helper ()
  '(sera:-> ((array alex:non-negative-fixnum (*))
             alex:non-negative-fixnum
             alex:non-negative-fixnum)
    (values alex:non-negative-fixnum &optional)))

(sera:-> dropwhile ((sera:-> (t) (values boolean &optional)) list)
         (values list &optional))
(defun dropwhile (f list)
  (cond
    ((null list) list)
    ((funcall f (car list))
     (dropwhile f (cdr list)))
    (t list)))

(sera:-> pass1!
         ((array alex:non-negative-fixnum (*)))
         (values (array alex:non-negative-fixnum (*)) &optional))
(defun pass1! (array)
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

;; Better to use FOLDL from stateless-iterators, but it's not in quicklisp
(sera:-> pass2! ((array alex:non-negative-fixnum (*)) dt-helper dt-helper)
         (values (array alex:non-negative-fixnum (*)) &optional))
(defun pass2! (array f sep)
  (let ((sp (list (cons 0 0))))
    (loop for i fixnum from 1 below (length array)
          for %sp = (dropwhile
                     (lambda (point)
                       (let ((f1 (funcall f array (cdr point) (car point)))
                             (f2 (funcall f array (cdr point) i)))
                         (> f1 f2)))
                     sp)
          do
          (setq sp
                (if (null %sp)
                    (list (cons i 0))
                    (let ((w (1+ (funcall sep array (caar %sp) i))))
                      (if (< w (length array))
                        (cons (cons i w) %sp) %sp)))))
    (loop with copy = (copy-seq array)
          for i fixnum from (1- (length array)) downto 0 do
          (setf (aref array i)
                (funcall f copy i (caar sp)))
          (when (= i (cdar sp))
            (pop sp))))
  array)

(declaim (ftype dt-helper distance-edt distance-cdt sep-edt sep-cdt))

(defun distance-cdt (array x i)
  (max (aref array i)
       (abs (- x i))))

(defun distance-edt (array x i)
  (+ (expt (aref array i) 2)
     (expt (- x i) 2)))

(defun sep-cdt (array i u)
  (if (<= (aref array i)
          (aref array u))
      (max (+ i (aref array u))
           (floor (+ i u) 2))
      (min (- u (aref array i))
           (floor (+ i u) 2))))

(defun sep-edt (array i u)
  (nth-value
   0 (floor
      (- (+ (expt u 2)
            (expt (aref array u) 2))
         (+ (expt i 2)
            (expt (aref array i) 2)))
      (* 2 (- u i)))))

(defun choose-pass2 (type)
  (declare (type (member :cdt :mdt :edt) type))
  (ecase type
    (:mdt #'pass1!)
    (:cdt (lambda (array) (pass2! array #'distance-cdt #'sep-cdt)))
    (:edt (lambda (array) (pass2! array #'distance-edt #'sep-edt)))))

(sera:-> transpose ((simple-array alex:non-negative-fixnum (* *)))
         (values (simple-array alex:non-negative-fixnum (* *)) &optional))
(defun transpose (array)
  (declare (optimize (speed 3)))
  (let* ((n (array-dimension array 0))
         (m (array-dimension array 1))
         (result (make-array (list m n) :element-type 'alex:non-negative-fixnum)))
    (loop for i fixnum below n do
          (loop for j fixnum below m do
                (setf (aref result j i) (aref array i j))))
    result))

(sera:-> distance-transform
         (binary-image &key (:type symbol) (:feature bit))
         (values (simple-array alex:non-negative-fixnum (* *)) &optional))
(defun distance-transform (image &key (type :edt) (feature 1))
  "Perform distance transform on a binary image. A distance to the
closest pixels with the value FEATURE is calculate for each pixels
in the image.

TYPE can be either :MDT (Manhattan distance transform), :CDT
(Chessboard distance transform) or :EDT (squared Euclidean distance)."
  (declare (optimize (speed 3)))
  (with-image-definition (image width height pixels)
    (let ((pass2! (choose-pass2 type))
          (distances (make-array (array-dimensions pixels)
                                 :element-type 'alex:non-negative-fixnum)))
      ;; Initialize the array with distances
      (map-into (aops:flatten distances)
                (lambda (x)
                  (declare (type bit x))
                  ;; FIXME: 2^30 is enough for pictures of size 30000x30000
                  (if (= x feature) 0 (ash 1 30)))
                (aops:flatten pixels))
      ;; Walk through the rows of the array and calculate MDT for each
      ;; row separately.
      (dotimes (row height)
        (pass1! (make-array width
                            :element-type 'alex:non-negative-fixnum
                            :displaced-to distances
                            :displaced-index-offset (* row width))))
      ;; Now walk through the columns. Have to permute the array for that :(
      (let ((transposition (transpose distances)))
        (dotimes (column width)
          (funcall pass2!
                   (make-array height
                               :element-type 'alex:non-negative-fixnum
                               :displaced-to transposition
                               :displaced-index-offset (* column height))))
        (transpose transposition)))))

;; =========
;; Thinning
;; =========
;;
;; Zicheng Guo & Richard W. Hall
;; https://dl.acm.org/doi/pdf/10.1145/62065.62074
(sera:-> thinning-pass-guo ((simple-array bit (* *)) boolean)
         (values (simple-array bit (* *)) boolean &optional))
(defun thinning-pass-guo (pixels odd-iteration-p)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension pixels 0))
         (width  (array-dimension pixels 1))
         (new-pixels (make-array (list height width) :element-type 'bit))
         (changed nil))
    (do-rectangle ((y x) (height width))
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
          (if (and (zerop o) (= c 1) (<= 2 n 3))
              (setf (aref new-pixels y x) 0
                    changed t)
              (setf (aref new-pixels y x) 1)))))
    (values new-pixels changed)))

;; Zhang & Suen
;; https://dl.acm.org/doi/10.1145/357994.358023
(sera:-> thinning-pass-zhang ((simple-array bit (* *)) boolean)
         (values (simple-array bit (* *)) boolean &optional))
(defun thinning-pass-zhang (pixels odd-iteration-p)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension pixels 0))
         (width  (array-dimension pixels 1))
         (new-pixels (make-array (list height width) :element-type 'bit))
         (changed nil))
    (do-rectangle ((y x) (height width))
      (unless (zerop (aref pixels y x))
        (let* ((p2 (aref pixels (mod (+ y -1) height) (mod (+ x  0) width)))
               (p3 (aref pixels (mod (+ y -1) height) (mod (+ x +1) width)))
               (p4 (aref pixels (mod (+ y  0) height) (mod (+ x +1) width)))
               (p5 (aref pixels (mod (+ y +1) height) (mod (+ x +1) width)))
               (p6 (aref pixels (mod (+ y +1) height) (mod (+ x  0) width)))
               (p7 (aref pixels (mod (+ y +1) height) (mod (+ x -1) width)))
               (p8 (aref pixels (mod (+ y  0) height) (mod (+ x -1) width)))
               (p9 (aref pixels (mod (+ y -1) height) (mod (+ x -1) width)))

               (b (+ p2 p3 p4 p5 p6 p7 p8 p9))
               (a (+ (if (and (= p2 0) (= p3 1)) 1 0)
                     (if (and (= p3 0) (= p4 1)) 1 0)
                     (if (and (= p4 0) (= p5 1)) 1 0)
                     (if (and (= p5 0) (= p6 1)) 1 0)
                     (if (and (= p6 0) (= p7 1)) 1 0)
                     (if (and (= p7 0) (= p8 1)) 1 0)
                     (if (and (= p8 0) (= p9 1)) 1 0)
                     (if (and (= p9 0) (= p2 1)) 1 0)))
               (c1 (if odd-iteration-p
                       (zerop (logand p2 p4 p6))
                       (zerop (logand p2 p4 p8))))
               (c2 (if odd-iteration-p
                       (zerop (logand p4 p6 p8))
                       (zerop (logand p2 p6 p8)))))

          (if (and (<= 2 b 6) (= a 1) c1 c2)
              (setf (aref new-pixels y x) 0
                    changed t)
              (setf (aref new-pixels y x) 1)))))
    (values new-pixels changed)))

(deftype thinning-type () '(member :guo :zhang))

(sera:-> thinning-pass ((simple-array bit (* *)) boolean thinning-type)
         (values (simple-array bit (* *)) boolean &optional))
(defun thinning-pass (pixels odd-iteration-p type)
  (ecase type
    (:zhang (thinning-pass-zhang pixels odd-iteration-p))
    (:guo   (thinning-pass-guo   pixels odd-iteration-p))))

(sera:-> thin (binary-image &optional thinning-type)
         (values binary-image &optional))
(defun thin (image &optional (type :zhang))
  "Perform thinning (extracting topological skeleton) of binary image."
  (declare (optimize (speed 3)))
  (labels ((%go (pixels odd-iteration-p)
             (multiple-value-bind (pixels changedp)
                 (thinning-pass pixels odd-iteration-p type)
               (if changedp (%go pixels (not odd-iteration-p)) pixels))))
    (make-binary-image-from-pixels
     (%go (image-pixels image) t))))
