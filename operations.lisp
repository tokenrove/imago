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


(defgeneric copy (dest src &key dest-x dest-y src-x src-y width height)
  (:documentation "Copies a rectangular region from image SRC to image DEST.
Both images must be large enough to contain the specified region at
the given positions. Both images must be of same type."))

(defgeneric scale (image width-factor height-factor)
  (:documentation "Returns an newly created image corresponding to the
IMAGE image, with its dimensions multiplied by the given factors."))

(defgeneric resize (image new-width new-height)
  (:documentation "Returns an newly created image corresponding to the
IMAGE image, with given dimensions."))

(defgeneric flip (dest image axis)
  (:documentation "Flips an image. AXIS may be either :HORIZONTAL or
:VERTICAL. DEST must be either an image of same type and dimensions as
IMAGE, or NIL. Returns the resulting image."))


(defmethod copy ((dest (eql nil)) (src image)
                 &key (dest-x 0) (dest-y 0) (src-x 0) (src-y 0) width height)
  (declare (ignore dest-x dest-y))
  (let ((dest (make-instance (class-of src)
                             :width (or width (image-width src))
                             :height (or height (image-height src)))))
    (copy dest src :src-x src-x :src-y src-y :width width :height height)))

(defmethod copy :around ((dest (eql nil)) (src indexed-image)
                         &key dest-x dest-y src-x src-y width height)
  (declare (ignore dest-x dest-y src-x src-y width height))
  (let ((dest (call-next-method))
        (colormap (copy-seq (image-colormap src))))
    (setf (slot-value dest 'image-colormap) colormap)
    dest))

(defmethod copy ((dest (eql nil)) (src planar-image)
                 &key (dest-x 0) (dest-y 0) (src-x 0) (src-y 0) width height)
  (declare (ignore dest-x dest-y))
  (let ((dest (make-instance (class-of src)
                             :width (or width (image-width src))
                             :height (or height (image-height src))
                             :plane-count (image-plane-count src)))
        (colormap (copy-seq (image-colormap src))))
    (setf (slot-value dest 'image-colormap) colormap)
    (copy dest src :src-x src-x :src-y src-y :width width :height height)))

(defmethod copy ((dest image) (src image)
                 &key (dest-x 0) (dest-y 0) (src-x 0) (src-y 0) width height)
  (assert (eq (class-of dest) (class-of src)))
  (let ((dest-width (image-width dest))
        (dest-height (image-height dest))
        (src-width (image-width src))
        (src-height (image-height src)))
    (assert (and (< -1 src-x src-width)
                 (< -1 src-y src-height)))
    (assert (and (< dest-x dest-width)
                 (< dest-y dest-height)))
    (when (null width)
      (setf width (- src-width src-x)))
    (when (null height)
      (setf height (- src-height src-y)))
    (when (>= (+ src-x width) src-width)
      (decf width (- (+ src-x width) src-width)))
    (when (>= (+ dest-x width) dest-width)
      (decf width (- (+ dest-x width) dest-width)))
    (when (< dest-x 0)
      (incf src-x (- dest-x))
      (decf width (- dest-x))
      (setf dest-x 0))
    (when (>= (+ src-y height) src-height)
      (decf height (- (+ src-y height) src-height)))
    (when (>= (+ dest-y height) dest-height)
      (decf height (- (+ dest-y height) dest-height)))
    (when (< dest-y 0)
      (incf src-y (- dest-y))
      (decf height (- dest-y))
      (setf dest-y 0))
    (loop repeat height
          as dest-x2 = dest-x
          as src-x2 = src-x
          do (loop repeat width
                   do (setf (image-pixel dest dest-x2 dest-y)
                            (image-pixel src src-x2 src-y))
                      (incf dest-x2)
                      (incf src-x2))
             (incf dest-y)
             (incf src-y)))
  dest)

(defmethod scale ((image image) width-factor height-factor)
  (let ((width (image-width image))
        (height (image-height image)))
    (resize image
            (floor (* width width-factor))
            (floor (* height height-factor)))))

(defmethod resize ((image image) new-width new-height)
  (let* ((dest (make-instance (class-of image)
                              :width new-width :height new-height))
         (dest-pixels (image-pixels dest)))
    (with-image-definition (image image-width image-height image-pixels)
      (multiple-value-bind (y-step y-errinc)
          (floor image-height new-height)
        (multiple-value-bind (x-step x-errinc)
            (floor image-width new-width)
          (let ((y-err (floor y-errinc 2))
                (image-line-index 0)
                (dest-index 0))
            (dotimes (y new-height)
              (let ((x-err (floor x-errinc 2))
                    (image-index image-line-index))
                (dotimes (x new-width)
                  (setf (row-major-aref dest-pixels dest-index)
                        (row-major-aref image-pixels image-index))
                  (incf dest-index)
                  (incf image-index x-step)
                  (incf x-err x-errinc)
                  (when (>= x-err new-width)
                    (incf image-index)
                    (decf x-err new-width))))
              (incf image-line-index (* y-step image-width))
              (incf y-err y-errinc)
              (when (>= y-err new-height)
                (incf image-line-index image-width)
                (decf y-err new-height)))))))
    dest))

(defmethod flip ((dest (eql nil)) (image image) axis)
  (let ((dest (make-similar-image image)))
    (flip dest image axis)))

(defmethod flip ((dest image) (image image) (axis (eql :horizontal)))
  (let ((height (image-height image)))
    (do-image-pixels (dest pixel x y)
      (setf pixel (image-pixel image x (- height y 1)))))
  dest)

(defmethod flip ((dest image) (image image) (axis (eql :vertical)))
  (let ((width (image-width image)))
    (do-image-pixels (dest pixel x y)
      (setf pixel (image-pixel image (- width x 1) y))))
  dest)


