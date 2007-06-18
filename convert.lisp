;;; IMAGO library
;;; Image format conversions
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defgeneric convert-to-rgb (image))

(defgeneric convert-to-grayscale (image))

(defgeneric convert-to-indexed (image))

(defgeneric convert-to-planar (image))


(defmethod convert-to-rgb ((image indexed-image))
  (let* ((width (image-width image))
         (height (image-height image))
         (colormap (image-colormap image))
         (pixels (image-pixels image))
         (result (make-instance 'rgb-image
                                :width width :height height))
         (result-pixels (image-pixels result)))
    (dotimes (i (* width height))
      (let ((color-index (row-major-aref pixels i)))
        (setf (row-major-aref result-pixels i)
              (aref colormap color-index))))
    result))

(defmethod convert-to-rgb ((image grayscale-image))
  (let* ((width (image-width image))
         (height (image-height image))
         (pixels (image-pixels image))
         (result (make-instance 'rgb-image
                                :width width :height height))
         (result-pixels (image-pixels result)))
    (dotimes (i (* width height))
      (let ((gray (gray-intensity (row-major-aref pixels i))))
        (setf (row-major-aref result-pixels i)
              (make-color gray gray gray))))
    result))

(defmethod convert-to-grayscale ((image rgb-image))
  (let* ((width (image-width image))
         (height (image-height image))
         (pixels (image-pixels image))
         (result (make-instance 'grayscale-image
                                :width width :height height))
         (result-pixels (image-pixels result)))
    (dotimes (i (* width height))
      (let ((intensity (color-intensity (row-major-aref pixels i))))
        (setf (row-major-aref result-pixels i) (make-gray intensity))))
    result))

(defmethod convert-to-grayscale ((image indexed-image))
  (let* ((width (image-width image))
         (height (image-height image))
         (colormap (image-colormap image))
         (pixels (image-pixels image))
         (result (make-instance 'grayscale-image
                                :width width :height height))
         (result-pixels (image-pixels result)))
    (dotimes (i (* width height))
      (let ((color-index (row-major-aref pixels i)))
        (setf (row-major-aref result-pixels i)
              (make-gray (color-intensity (aref colormap color-index))))))
    result))

(defmethod convert-to-indexed ((image rgb-image))
  (error "Not implemented"))


(defmethod convert-to-indexed ((image grayscale-image))
  (let* ((width (image-width image))
         (height (image-height image))
         (pixels (image-pixels image))
         (colormap (make-simple-gray-colormap))
         (result (make-instance 'indexed-image
                                :width width :height height
                                :colormap colormap))
         (result-pixels (image-pixels result)))
    (dotimes (i (* width height))
      (setf (row-major-aref result-pixels i)
            (gray-intensity (row-major-aref pixels i))))
    result))


(defmethod convert-to-planar ((image image))
  (error "Not implemented"))