;;; IMAGO library
;;; TGA file handling
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defun read-tga (filespec)
  (with-open-file (stream filespec :direction :input
                   :element-type '(unsigned-byte 8))
    (let* ((id-field-length (read-byte stream))
           (colormap-type (read-byte stream))
           (type (read-byte stream))
           (colormap-origin (read-lsb-integer stream 2))
           (colormap-length (read-lsb-integer stream 2))
           (colormap-bpp (read-byte stream))
           (origin-x (read-lsb-integer stream 2))
           (origin-y (read-lsb-integer stream 2))
           (width (read-lsb-integer stream 2))
           (height (read-lsb-integer stream 2))
           (pixel-size (read-byte stream))
           (descriptor (read-byte stream))
           (alpha-size (ldb (byte 4 0) descriptor))
           (top-down-p (= (ldb (byte 1 5) descriptor) 1)))
      (declare (ignore colormap-type origin-x origin-y))
      (skip-bytes stream id-field-length)
      (let ((colormap (read-tga-colormap stream
                                         colormap-origin colormap-length
                                         colormap-bpp)))
        (case type
          (1 (let ((image (make-instance 'indexed-image
                                         :width width :height height
                                         :color-count (+ colormap-origin
                                                         colormap-length))))
               (replace (image-colormap image) colormap)
               (read-tga-data stream image width height top-down-p
                              'read-tga-color-index pixel-size)
               image))
          (2 (let ((image (make-instance 'rgb-image
                                         :width width :height height)))
               (read-tga-data stream image width height top-down-p
                              'read-tga-color pixel-size)
               (when (zerop alpha-size)
                 (do-image-pixels (image pixel x y)
                   (multiple-value-bind (r g b)
                       (color-rgb pixel)
                     (setf pixel (make-color r g b)))))
               image)))))))

(defun read-tga-colormap (stream origin length bpp)
  (loop with colormap = (make-array (+ origin length) :initial-element 0)
        for i from origin
        repeat length
        do (setf (aref colormap i) (read-tga-color stream bpp))
        finally (return colormap)))

(defun read-tga-color-index (stream bpp)
  (read-lsb-integer stream (floor bpp 8)))

(defun read-tga-color (stream bpp)
  (case bpp
    (16 (let ((raw (read-lsb-integer stream 2)))
          (make-color (ash (ldb (byte 5 10) 3) raw)
                      (ash (ldb (byte 5 5) 3) raw)
                      (ash (ldb (byte 5 0) 3) raw))))
    (24 (let ((raw (read-lsb-integer stream 3)))
          (make-color (ldb (byte 8 16) raw)
                      (ldb (byte 8 8) raw)
                      (ldb (byte 8 0) raw))))
    (32 (let ((raw (read-lsb-integer stream 4)))
          (make-color (ldb (byte 8 16) raw)
                      (ldb (byte 8 8) raw)
                      (ldb (byte 8 0) raw)
                      (ldb (byte 8 24) raw))))))

(defun read-tga-data (stream image width height top-down-p reader bpp)
  (loop with y-inc = (if top-down-p 1 -1)
        for y = (if top-down-p 0 (1- height)) then (+ y y-inc)
        repeat height
        do (loop for x below width
                 do (setf (image-pixel image x y)
                          (funcall reader stream bpp)))))

(register-image-reader '("tga" "TGA") #'read-tga)


(defun write-tga (image filespec &key (top-down-p nil))
  (with-open-file (stream filespec :direction :output :if-exists :supersede
                   :element-type '(unsigned-byte 8))
    (write-tga-to-stream image stream top-down-p))
  image)

(defgeneric write-tga-to-stream (image stream top-down-p))

(defmethod write-tga-to-stream ((image rgb-image) stream top-down-p)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 2 stream)
  (loop repeat 5
        do (write-byte 0 stream))
  (write-lsb-integer 0 stream 2)
  (write-lsb-integer 0 stream 2)
  (write-lsb-integer (image-width image) stream 2)
  (write-lsb-integer (image-height image) stream 2)
  (write-byte 32 stream)
  (write-byte (+ 8 (if top-down-p (ash 1 5) 0)) stream)
  (write-tga-data image top-down-p
                  (lambda (pixel)
                    (write-byte (color-blue pixel) stream)
                    (write-byte (color-green pixel) stream)
                    (write-byte (color-red pixel) stream)
                    (write-byte (color-alpha pixel) stream))))

(defmethod write-tga-to-stream ((image indexed-image) stream top-down-p)
  (let* ((colormap (image-colormap image))
         (colormap-length (length colormap))
         (pixel-size (cond ((<= (length colormap) 256) 8)
                           ((<= (length colormap) 65536) 16)
                           (t 32))))
    (write-byte 0 stream)
    (write-byte 1 stream)
    (write-byte 1 stream)
    (write-lsb-integer 0 stream 2)
    (write-lsb-integer colormap-length stream 2)
    (write-lsb-integer 32 stream 2)
    (write-lsb-integer 0 stream 2)
    (write-lsb-integer 0 stream 2)
    (write-lsb-integer (image-width image) stream 2)
    (write-lsb-integer (image-height image) stream 2)
    (write-byte pixel-size stream)
    (write-byte (if top-down-p (ash 1 5) 0) stream)
    (write-tga-data image top-down-p
                    (lambda (pixel)
                      (write-lsb-integer pixel stream
                                         (floor pixel-size 8))))))

(defun write-tga-data (image top-down-p pixel-writer)
  (loop with width = (image-width image)
        with height = (image-height image)
        for y = (if top-down-p 0 (1- height))
        then (if top-down-p (1+ y) (1- y))
        repeat height
        do (loop for x below width
                 do (funcall pixel-writer (image-pixel image x y)))))
