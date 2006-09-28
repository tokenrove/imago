;;; IMAGO library
;;; Image data structure definitions
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defclass image ()
  ()
  (:documentation "The protocol class for images."))

(defgeneric image-pixels (image)
  (:documentation "Returns a rectangular array of the image pixels."))

(defgeneric image-width (image)
  (:documentation "Returns the width of the image."))

(defgeneric image-height (image)
  (:documentation "Returns the height of the image."))

(defgeneric image-pixel (image x y)
  (:documentation "Returns the color of the pixel at specified coordinates
in the image."))

(defgeneric (setf image-pixel) (pixel image x y)
  (:documentation "Sets the color of the pixel at specified coordinates
in the image."))

(defgeneric pixel-size (image)
  (:documentation "Returns the number of bytes used to represent a pixel."))


(defmethod image-width ((image image))
  (second (array-dimensions (image-pixels image))))

(defmethod image-height ((image image))
  (first (array-dimensions (image-pixels image))))

(defmethod image-pixel ((image image) x y)
  (aref (image-pixels image) y x))

(defmethod (setf image-pixel) (pixel (image image) x y)
  (setf (aref (image-pixels image) y x) pixel))

(defmethod print-object ((object image) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~Dx~D)" (image-width object) (image-height object))))


(defclass rgb-image (image)
  ((pixels :type (simple-array rgb-pixel (* *))
           :reader image-pixels))
  (:documentation "The class for RGB images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters."))

(defmethod initialize-instance :after ((image rgb-image) &rest initargs
                                       &key width height pixels)
  (declare (ignore initargs))
  (cond ((not (null pixels))
         (setf (slot-value image 'pixels) pixels))
        ((and (numberp width) (numberp height))
         (setf (slot-value image 'pixels)
               (make-array (list height width)
                           :element-type 'rgb-pixel)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image rgb-image)) 4)


(defclass grayscale-image (image)
  ((pixels :type (simple-array grayscale-pixel (* *))
           :reader image-pixels))
  (:documentation "The class for grayscale images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters."))

(defmethod initialize-instance :after ((image grayscale-image) &rest initargs
                                       &key width height pixels)
  (declare (ignore initargs))
  (cond ((not (null pixels))
         (setf (slot-value image 'pixels) pixels))
        ((and (numberp width) (numberp height))
         (setf (slot-value image 'pixels)
               (make-array (list height width)
                           :element-type 'grayscale-pixel)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image grayscale-image)) 2)


(defclass indexed-image (image)
  ((pixels :type (simple-array indexed-pixel (* *))
           :reader image-pixels)
   (colormap :initarg :colormap :reader image-colormap))
  (:documentation "The class for indexed images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters. Also accepts a :COLOR-COUNT keyword parameter, to specify
the image color count (256 by default)."))

(defmethod initialize-instance :after ((image indexed-image) &rest initargs
                                       &key width height pixels
                                            colormap (color-count 256))
  (declare (ignore initargs)
	   (type fixnum color-count))
  (cond ((not (null colormap))
         (setf (slot-value image 'colormap) colormap))
        ((and (numberp color-count) (<= color-count 256))
         (setf (slot-value image 'colormap)
               (make-array color-count :initial-element 0)))
        (t (error "Invalid initialization arguments")))
  (cond ((not (null pixels))
         (setf (slot-value image 'pixels) pixels))
        ((and (numberp width) (numberp height))
         (setf (slot-value image 'pixels)
               (make-array (list height width)
                           :element-type 'indexed-pixel)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image indexed-image)) 1)


(defclass planar-image (image)
  ;; XXX: I couldn't get a tighter type specifier to match this,
  ;; because of the parameter to planar-pixel.
  ((pixels :type simple-array
           :reader image-pixels)
   (plane-count :type integer :reader image-plane-count)
   (colormap :initarg :colormap :reader image-colormap))
  (:documentation "The class for planar images.  Image dimensions and
plane count must be provided to MAKE-INSTANCE, through the :WIDTH,
:HEIGHT, and :PLANE-COUNT keyword parameters, respectively."))

(defmethod initialize-instance :after ((image planar-image) &rest initargs
                                       &key width height plane-count pixels
                                       colormap)
  (declare (ignore initargs))
  (unless (numberp plane-count)
    (error "Invalid initialization arguments (you must specify a plane count for this type of image)"))
  (setf (slot-value image 'plane-count) plane-count)

  (cond ((not (null colormap))
         (assert (= (length colormap) (ash 1 plane-count)))
         (setf (slot-value image 'colormap) colormap))
        (t
         ;; XXX shouldn't the initial element of the colormap be a
         ;; color (like +black+, not 0)?
         (setf (slot-value image 'colormap)
               (make-array (ash 1 plane-count) :initial-element 0))))
  (cond ((not (null pixels))
         (setf (slot-value image 'pixels) pixels))
        ((and (numberp width) (numberp height))
         (setf (slot-value image 'pixels)
               (make-array (list height width)
                           :element-type `(planar-pixel ,plane-count)
                           :initial-element 0)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image planar-image))
  (/ (image-plane-count image) 8))

