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

(defgeneric image-pixel (image x y)
  (:documentation "Returns the color of the pixel at specified coordinates
in the image."))

(defgeneric (setf image-pixel) (pixel image x y)
  (:documentation "Sets the color of the pixel at specified coordinates
in the image."))

(defgeneric pixel-size (image)
  (:documentation "Returns the number of bytes used to represent a pixel."))


(defmethod image-pixel ((image image) x y)
  (aref (image-pixels image) y x))

(defmethod (setf image-pixel) (pixel (image image) x y)
  (setf (aref (image-pixels image) y x) pixel))


(defmacro add-generic-initializer (image-type pixel-type init-color-form)
  `(defmethod initialize-instance :after ((image ,image-type) &rest initargs
                                          &key width height pixels (initial-color ,init-color-form))
     (declare (ignore initargs))
     (cond (pixels
            (setf (image-pixels image) pixels))
           ((and (integerp width)
                 (integerp height))
            (setf (image-pixels image)
                  (make-array (list height width)
                              :element-type ',pixel-type
                              :initial-element initial-color)))
           (t (error "Invalid initialization arguments")))))

(defclass rgb-image (image)
  ((pixels :type     (simple-array rgb-pixel (* *))
           :accessor image-pixels))
  (:documentation "The class for RGB images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters."))

(add-generic-initializer rgb-image rgb-pixel (make-color 0 0 0))
(defmethod pixel-size ((image rgb-image)) 4)


(defclass grayscale-image (image)
  ((pixels :type     (simple-array grayscale-pixel (* *))
           :accessor image-pixels))
  (:documentation "The class for grayscale images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters."))

(add-generic-initializer grayscale-image grayscale-pixel (make-gray 0))
(defmethod pixel-size ((image grayscale-image)) 2)


(defclass indexed-image (image)
  ((pixels   :type     (simple-array indexed-pixel (* *))
             :accessor image-pixels)
   (colormap :initarg  :colormap
             :accessor image-colormap))
  (:documentation "The class for indexed images. Image dimensions must be
provided to MAKE-INSTANCE, through the :WIDTH and :HEIGHT keyword
parameters. Also accepts a :COLOR-COUNT keyword parameter, to specify
the image color count (256 by default)."))

(defmethod initialize-instance :after ((image indexed-image) &rest initargs
                                       &key width height pixels
                                            colormap (color-count 256))
  (declare (ignore initargs)
	   (type fixnum color-count))
  (cond (colormap
         (setf (image-colormap image) colormap))
        ((and (integerp color-count) (<= color-count 256))
         (setf (image-colormap image)
               (make-array color-count :initial-element 0)))
        (t (error "Invalid initialization arguments")))
  (cond (pixels
         (setf (image-pixels image) pixels))
        ((and (integerp width) (integerp height))
         (setf (image-pixels image)
               (make-array (list height width)
                           :element-type 'indexed-pixel)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image indexed-image)) 1)


(defclass planar-image (image)
  ;; XXX: I couldn't get a tighter type specifier to match this,
  ;; because of the parameter to planar-pixel.
  ((pixels      :type     simple-array
                :accessor image-pixels)
   (plane-count :type     integer
                :accessor image-plane-count)
   (colormap    :initarg  :colormap
                :accessor image-colormap))
  (:documentation "The class for planar images.  Image dimensions and
plane count must be provided to MAKE-INSTANCE, through the :WIDTH,
:HEIGHT, and :PLANE-COUNT keyword parameters, respectively."))

(defmethod initialize-instance :after ((image planar-image) &rest initargs
                                       &key width height plane-count pixels
                                       colormap)
  (declare (ignore initargs))
  (unless (integerp plane-count)
    (error "Invalid initialization arguments (you must specify a plane count for this type of image)"))
  (setf (image-plane-count image) plane-count)

  (cond (colormap
         (assert (= (length colormap) (ash 1 plane-count)))
         (setf (image-colormap image) colormap))
        (t
         ;; XXX shouldn't the initial element of the colormap be a
         ;; color (like +black+, not 0)?
         (setf (image-colormap image)
               (make-array (ash 1 plane-count) :initial-element 0))))
  (cond (pixels
         (setf (image-pixels image) pixels))
        ((and (integerp width) (integerp height))
         (setf (image-pixels image)
               (make-array (list height width)
                           :element-type `(planar-pixel ,plane-count)
                           :initial-element 0)))
        (t (error "Invalid initialization arguments"))))

(defmethod pixel-size ((image planar-image))
  (/ (image-plane-count image) 8))

(defclass binary-image (image)
  ((pixels :type     (simple-array bit (* *))
           :accessor image-pixels))
  (:documentation "The class for binary images whose pixels are just 0
or 1. Image dimensions must be provided to MAKE-INSTANCE, through
the :WIDTH and :HEIGHT keyword parameters."))

(add-generic-initializer binary-image bit 0)
;; Better leave this undefined
#+nil
(defmethod pixel-size ((image binary-image)) 1)

;; Widht and height readers

(sera:-> image-width  (image) (values alex:positive-fixnum &optional))
(sera:-> image-height (image) (values alex:positive-fixnum &optional))

(defun image-height (image)
  (declare (type image image))
  (array-dimension (image-pixels image) 0))

(defun image-width (image)
  (declare (type image image))
  (array-dimension (image-pixels image) 1))

(defmethod print-object ((object image) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~Dx~D)" (image-width object) (image-height object))))
