;;; IMAGO library
;;; JPG file handling using cl-jpeg
;;;
;;; Copyright (C) 2020  Vasily Postnicov <shamaz.mazum@gmail.com>
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

;; Reading
(defgeneric read-jpg-pixel (image array idx)
  (:documentation "Read a pixel from an array with jpeg data"))

(defmethod read-jpg-pixel ((image grayscale-image) array idx)
  (setf (row-major-aref (image-pixels image) idx)
        (make-gray (aref array idx))))

(defmethod read-jpg-pixel ((image rgb-image) array idx)
  (let ((idx% (* 3 idx)))
    (setf (row-major-aref (image-pixels image) idx)
          (make-color (aref array (+ idx% 2))
                      (aref array (+ idx% 1))
                      (aref array (+ idx% 0))))))

(defun read-jpg-from-stream (stream)
  "Read grayscale or colorful jpeg image using cl-jpeg. Colorful images are
converted to RGB colorspace. Stream must be a stream of unsigned octets."
  (multiple-value-bind (data height width ncomp)
      (cl-jpeg:decode-stream stream)
    (let ((image (make-instance
                  (case ncomp
                    (1 'grayscale-image)
                    (3 'rgb-image)
                    (t (error 'decode-error
                              :format-control "Unsupported number of color components: ~d"
                              :format-arguments (list ncomp))))
                  :width  width
                  :height height)))

      (dotimes (idx (* width height))
        (read-jpg-pixel image data idx))
      image)))

(def-reader-from-file read-jpg read-jpg-from-stream
  "Read grayscale or colorful jpeg image using cl-jpeg. Colorful images are
converted to RGB colorspace.")

;; Writing
(defgeneric write-jpg-pixel (image array idx)
  (:documentation "Write a pixel at row-major index IDX to the one
dimensional array"))

(defmethod write-jpg-pixel ((image grayscale-image) array idx)
  (setf (aref array idx)
        (gray-intensity
         (row-major-aref
          (image-pixels image) idx))))

(defmethod write-jpg-pixel ((image rgb-image) array idx)
  (let ((idx% (* 3 idx))
        (pixel (row-major-aref (image-pixels image) idx)))
    (setf (aref array (+ idx% 0))
          (color-blue pixel)
          (aref array (+ idx% 1))
          (color-green pixel)
          (aref array (+ idx% 2))
          (color-red pixel))))

(defun write-jpg-to-stream (image stream &key (quality 64))
  "Write jpeg image IMAGE to an octet stream STREAM. QUALITY is an
integer from 1 to 64 where 64 is default and the best quality."
  (declare (type (or rgb-image grayscale-image) image)
           (type stream stream)
           (type (integer 1 64) quality))
  (let* ((ncomp (1- (pixel-size image))) ; Skip alpha
         (width (image-width image))
         (height (image-height image))
         (data-size (* ncomp width height))
         (data (make-array data-size :element-type '(unsigned-byte 8))))
    (dotimes (idx (* width height))
      (write-jpg-pixel image data idx))
    (cl-jpeg:encode-image-stream
     stream data ncomp height width
     :q-factor quality
     ;; Seems like a bug in cl-jpeg that I need to specify this
     ;; manually.
     :q-tabs (if (= ncomp 1)
                 (vector cl-jpeg::+q-luminance-hi+)
                 (vector cl-jpeg::+q-luminance-hi+
                         cl-jpeg::+q-chrominance-hi+))))
  image)

(def-writer-to-file
    write-jpg write-jpg-to-stream ((quality 64))
    "Write imago image to jpeg file. QUALITY is an integer from 1 to 64
where 64 is default and the best quality.")

(register-image-io-functions '("jpg" "jpeg")
                             :reader #'read-jpg
                             :writer #'write-jpg)
