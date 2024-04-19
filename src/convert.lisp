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


(defgeneric convert-to-rgb (image)
  (:method ((image image))
    (error 'not-implemented))
  (:method ((image rgb-image))
    image))

(defgeneric convert-to-grayscale (image)
  (:method ((image image))
    (error 'not-implemented))
  (:method ((image grayscale-image))
    image))

(defgeneric convert-to-indexed (image)
  (:method ((image image))
    (error 'not-implemented))
  (:method ((image indexed-image))
    image))

(defgeneric convert-to-planar (image)
  (:method ((image image))
    (error 'not-implemented))
  (:method ((image planar-image))
    image))

(defgeneric convert-to-binary (image threshold)
  (:method ((image image) threshold)
    (error 'not-implemented))
  (:documentation "Convert to binary image by thresholding pixels of
the image"))


(defmethod convert-to-rgb ((image indexed-image))
  (let ((colormap (image-colormap image))
        (pixels   (image-pixels   image)))
    (make-rgb-image-from-pixels
     ;; XXX: What does a colormap really contain?
     (aops:vectorize* 'imago:rgb-pixel
         (pixels)
       (aref colormap pixels)))))

(defmethod convert-to-rgb ((image grayscale-image))
  (let ((pixels (image-pixels image)))
    (make-rgb-image-from-pixels
     (aops:vectorize* 'imago:rgb-pixel
         (pixels)
       (let ((alpha     (gray-alpha     pixels))
             (intensity (gray-intensity pixels)))
         (make-color intensity intensity intensity alpha))))))

(defmethod convert-to-rgb ((image binary-image))
  (let ((pixels (image-pixels image)))
    (make-rgb-image-from-pixels
     (aops:vectorize* 'imago:rgb-pixel
         (pixels)
       (let ((intensity (* 255 pixels)))
         (make-color intensity intensity intensity))))))

(defmethod convert-to-grayscale ((image rgb-image))
  (let ((pixels (image-pixels image)))
    (make-grayscale-image-from-pixels
     (aops:vectorize* 'imago:grayscale-pixel
         (pixels)
       (let ((intensity (color-intensity pixels))
             (alpha     (color-alpha     pixels)))
         (make-gray intensity alpha))))))

(defmethod convert-to-grayscale ((image indexed-image))
  (let ((colormap (image-colormap image))
        (pixels   (image-pixels   image)))
    (make-grayscale-image-from-pixels
     ;; XXX: What does a colormap really contain?
     (aops:vectorize* 'imago:grayscale-pixel
         (pixels)
       (make-gray (color-intensity (aref colormap pixels)))))))

(defmethod convert-to-grayscale ((image binary-image))
  (let ((pixels (image-pixels image)))
    (make-grayscale-image-from-pixels
     (aops:vectorize* 'imago:grayscale-pixel
         (pixels)
       (make-gray (* 255 pixels))))))

;; TODO: Make more comprehensive
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

(defmethod convert-to-binary ((image rgb-image) (threshold integer))
  (let ((pixels (image-pixels image)))
    (make-binary-image-from-pixels
     (aops:vectorize* 'bit
         (pixels)
       (let ((intensity (color-intensity pixels)))
         (if (< intensity threshold) 0 1))))))

(defmethod convert-to-binary ((image grayscale-image) (threshold integer))
  (let ((pixels (image-pixels image)))
    (make-binary-image-from-pixels
     (aops:vectorize* 'bit
         (pixels)
       (let ((intensity (gray-intensity pixels)))
         (if (< intensity threshold) 0 1))))))
