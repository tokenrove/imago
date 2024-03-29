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

(defun crop (image x y width height)
  "Crops an image. The resulting image is WIDTHxHEIGHT
and its top left corner has coordinates (X, Y) in the original image
space. If crop rectangle is exceeding dimensions of the original image,
OPERATION-ERROR is signalled."
  (declare (type unsigned-byte x y width height)
           (type image image))
  (when (or
         (> (+ x width)  (image-width  image))
         (> (+ y height) (image-height image)))
    (error 'operation-error
           :format-control "Cannot crop at ~d,~d ~dx~d."
           :format-arguments (list x y width height)))
  (let ((dest (make-instance (class-of image)
                             :width  width
                             :height height)))
    (do-image-pixels (dest pixel x% y%)
      (setf pixel (image-pixel image
                               (+ x x%)
                               (+ y y%))))
    dest))

(declaim (inline resize-pixels))
(defun resize-pixels (pixel-type interpolator new-width new-height)
  (declare (type image-dimension new-width new-height)
           (type (sera:-> ((single-float 0f0 1f0)
                           (single-float 0f0 1f0))
                          (values t &optional))
                 interpolator))
  (let ((new-pixels (make-array (list new-height new-width)
                            :element-type pixel-type))
        (new-width%  (float new-width))
        (new-height% (float new-height)))
    (loop for y fixnum below new-height do
          (loop for x fixnum below new-width do
                (setf (aref new-pixels y x)
                      (funcall interpolator
                               (/ x new-width%)
                               (/ y new-height%)))))
    new-pixels))

(defgeneric %resize (method image new-width new-height))

(macrolet ((define-%resize-method (image-type image-cons pixel-type)
             `(defmethod %resize (method (image ,image-type) new-width new-height)
                (declare (optimize (speed 3)))
                (let ((interpolator (interpolator image method)))
                  (,image-cons
                   (resize-pixels ',pixel-type interpolator new-width new-height))))))
  (define-%resize-method rgb-image make-rgb-image-from-pixels rgb-pixel)
  (define-%resize-method grayscale-image make-grayscale-image-from-pixels grayscale-pixel)
  (define-%resize-method binary-image make-binary-image-from-pixels bit))

(defmethod %resize ((method (eql :nearest-neighbor)) (image indexed-image) new-width new-height)
  (declare (optimize (speed 3)))
  (let ((interpolator (interpolator image method)))
    (make-instance 'indexed-image
                   :pixels (resize-pixels 'indexed-pixel interpolator new-width new-height)
                   :colormap (image-colormap image))))

;; METHOD = :BOX-SAMPLING is an alias for DOWNSCALE
(defmethod %resize ((method (eql :box-sampling)) (image image) new-width new-height)
  (downscale image new-width new-height))

(defun resize (image new-width new-height
               &key (interpolation *default-interpolation*))
  "Returns an newly created image corresponding to the
IMAGE image, with given dimensions. Currently supported
INTERPOLATION methods are :NEAREST-NEIGHBOR, :BILINEAR,
:BICUBIC and :BOX-SAMPLING. When :BOX-SAMPLING is passed RESIZE just
calls DOWNSCALE function which uses box sampling to downscale
images. This last method can only be used for downscaling."
  (%resize interpolation image new-width new-height))

(defun scale (image width-factor height-factor
              &key (interpolation *default-interpolation*))
  "Returns an newly created image corresponding to the
IMAGE image, with its dimensions multiplied by the given
factors. INTERPOLATION is pixel interpolation method, see RESIZE for
more details."
  (declare (type image image))
  (let ((width (image-width image))
        (height (image-height image)))
    (resize image
            (floor (* width width-factor))
            (floor (* height height-factor))
            :interpolation interpolation)))

(defgeneric invert (image)
  (:documentation "Create a new image with colors of the supplied
image being inverted."))

(defmethod invert ((image rgb-image))
  (let ((pixels (image-pixels image)))
    (make-rgb-image-from-pixels
     (aops:vectorize* 'rgb-pixel
         (pixels)
       (invert-color pixels)))))

(defmethod invert ((image grayscale-image))
  (let ((pixels (image-pixels image)))
    (make-grayscale-image-from-pixels
     (aops:vectorize* 'grayscale-pixel
         (pixels)
       (invert-gray pixels)))))

(defmethod invert ((image binary-image))
  (let ((pixels (image-pixels image)))
    (make-binary-image-from-pixels
     (aops:vectorize* 'bit
         (pixels)
       (- 1 pixels)))))
