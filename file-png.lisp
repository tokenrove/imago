;;; IMAGO library
;;; PNG file handling
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defparameter +png-signature+ '#(137 80 78 71 13 10 26 10))

(defparameter +png-ihdr-chunk-type+ #x49484452)
(defparameter +png-idat-chunk-type+ #x49444154)
(defparameter +png-plte-chunk-type+ #x504C5445)
(defparameter +png-iend-chunk-type+ #x49454E44)

(defstruct png-descriptor
  width height
  depth color-type
  compression-method filter-method interlace-method)

(defun read-png (filespec)
  (with-open-file (stream filespec :direction :input
                   :element-type '(unsigned-byte 8))
    (read-png-signature stream)
    (let ((descriptor nil)
          (data (make-array (file-length stream)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0))
          (data-index 0)
          (colormap nil))
      (loop for chunk = (read-png-chunk stream)
            until (= (car chunk) +png-iend-chunk-type+)
            do (cond ((= (car chunk) +png-ihdr-chunk-type+)
                      (setf descriptor (decode-png-descriptor (cdr chunk))))
                     ((= (car chunk) +png-plte-chunk-type+)
                      (setf colormap (decode-png-colormap (cdr chunk))))
                     ((= (car chunk) +png-idat-chunk-type+)
                      (let* ((chunk-data (cdr chunk))
                             (chunk-length (length chunk-data)))
                        (incf (fill-pointer data) chunk-length)
                        (replace data chunk-data :start1 data-index)
                        (incf data-index chunk-length)))))
      (when (or (/= (png-descriptor-compression-method descriptor) 0)
                (/= (png-descriptor-filter-method descriptor) 0)
                (/= (png-descriptor-interlace-method descriptor) 0))
        (error "PNG: Format not recognized"))
      (let* ((raw-data (zlib:uncompress data))
             (pixels (decode-png-image descriptor raw-data)))
        (ecase (png-descriptor-color-type descriptor)
          ((2 6) (make-instance 'rgb-image :pixels pixels))
          ((0 4) (make-instance 'grayscale-image :pixels pixels))
          ((3) (make-instance 'indexed-image
                              :pixels pixels
                              :colormap colormap)))))))

(defun read-png-signature (stream)
  (let ((bytes (read-byte-array stream (length +png-signature+))))
    (when (mismatch bytes +png-signature+ :test #'=)
      (error "PNG: Invalid signature"))))

(defun read-png-chunk (stream)
  (let* ((length (read-msb-integer stream 4))
         (type (read-msb-integer stream 4))
         (data (read-byte-array stream length))
         (crc (read-msb-integer stream 4)))
    (declare (ignore crc))
    (cons type data)))

(defun decode-png-descriptor (data)
  (make-png-descriptor :width (logior (ash (aref data 0) 24)
                                      (ash (aref data 1) 16)
                                      (ash (aref data 2) 8)
                                      (aref data 3))
                       :height (logior (ash (aref data 4) 24)
                                       (ash (aref data 5) 16)
                                   (ash (aref data 6) 8)
                                   (aref data 7))
                       :depth (aref data 8)
                       :color-type (aref data 9)
                       :compression-method (aref data 10)
                       :filter-method (aref data 11)
                       :interlace-method (aref data 12)))

(defun decode-png-colormap (data)
  (let* ((length (floor (length data) 3))
         (colormap (make-array length)))
    (loop for i below length
          do (setf (aref colormap i)
                   (make-color (aref data (* i 3))
                               (aref data (+ (* i 3) 1))
                               (aref data (+ (* i 3) 2)))))
    colormap))

(defun decode-png-image (descriptor data)
  (let* ((width (png-descriptor-width descriptor))
         (height (png-descriptor-height descriptor))
         (color-type (png-descriptor-color-type descriptor))
         (depth (png-descriptor-depth descriptor))
         (samples-per-pixel (png-samples-per-pixel color-type))
         (samples (make-array (* width height samples-per-pixel)
                              :element-type '(unsigned-byte 16)))
         (data-bit-index 0))
    (loop with samples-index = 0
          for y below height
          for filter = (prog1
                         (read-png-sample data data-bit-index 8)
                         (incf data-bit-index 8))
          do (loop for x below (* width samples-per-pixel)
                   for sample = (prog1
                                  (read-png-sample data data-bit-index depth)
                                  (incf data-bit-index depth))
                   for left = (if (>= x samples-per-pixel)
                                  (aref samples (- samples-index
                                                   samples-per-pixel))
                                  0)
                   for up = (if (> y 0)
                                (aref samples
                                      (- samples-index
                                         (* width samples-per-pixel)))
                                0)
                   for upleft = (if (and (>= x samples-per-pixel) (> y 0))
                                    (aref samples
                                          (- samples-index
                                             (* width samples-per-pixel)
                                             samples-per-pixel))
                                    0)
                   for sample2 = (ecase filter
                                   (0 sample)
                                   (1 (mod (+ sample left) 256))
                                   (2 (mod (+ sample up) 256))
                                   (3 (mod (+ sample
                                              (floor (+ up left) 2)) 256))
                                   (4 (mod (+ sample
                                              (png-paeth left up upleft))
                                           256)))
                   do (setf (aref samples samples-index) sample2)
                      (incf samples-index))
             (unless (zerop (mod data-bit-index 8))
               (incf data-bit-index (- 8 (mod data-bit-index 8)))))
    (loop with samples-index = 0
          with pixels = (make-array (list height width)
                                    :element-type (ecase color-type
                                                    ((2 6) 'rgb-pixel)
                                                    ((0 4) 'grayscale-pixel)
                                                    ((3) 'indexed-pixel)))
          for y below height
          do (loop for x below width
                   do (macrolet ((next-byte ()
                                 `(ash (read-array-element
                                        samples samples-index)
                                   (- 8 depth))))
                        (setf (aref pixels y x)
                              (case color-type
                                (0 (make-gray (next-byte)))
                                (2 (make-color (next-byte)
                                               (next-byte)
                                               (next-byte)))
                                (3 (read-array-element samples samples-index))
                                (4 (make-gray (next-byte)
                                              (next-byte)))
                                (6 (make-color (next-byte)
                                               (next-byte)
                                               (next-byte)
                                               (next-byte)))))))
          finally (return pixels))))

(defun png-samples-per-pixel (color-type)
  (ecase color-type
    (0 1)
    (2 3)
    (3 1)
    (4 2)
    (6 4)))

(defun read-png-sample (data bit-index depth)
  (multiple-value-bind (byte-index bit)
      (floor bit-index 8)
    (let ((byte (aref data byte-index)))
      (ldb (byte depth (- 8 depth bit)) byte))))

(defun png-paeth (a b c)
  (let* ((p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(register-image-reader '("png" "PNG") #'read-png)


(defun write-png (image filespec)
  (with-open-file (stream filespec :direction :output :if-exists :supersede
                   :element-type '(unsigned-byte 8))
    (write-png-signature stream)
    (write-png-header-chunk stream image)
    (write-png-colormap-chunk stream image)
    (write-png-data-chunk stream image)
    (write-png-end-chunk stream)))

(defun write-png-signature (stream)
  (write-sequence +png-signature+ stream))

(defun write-png-chunk (data type stream)
  (let ((type-bytes (make-array 4))
        (crc 0))
    (loop for i below 4
          do (setf (aref type-bytes i) (ldb (byte 8 (* 8 (- 3 i))) type)))
    (setf crc (update-crc32 crc type-bytes))
    (setf crc (update-crc32 crc data))
    (write-msb-integer (length data) stream 4)
    (write-msb-integer type stream 4)
    (write-sequence data stream)
    (write-msb-integer crc stream 4)))

(defun write-png-header-chunk (stream image)
  (let ((data (make-array 13 :element-type '(unsigned-byte 8)))
        (width (image-width image))
        (height (image-height image)))
    (setf (aref data 0) (ldb (byte 8 24) width)
          (aref data 1) (ldb (byte 8 16) width)
          (aref data 2) (ldb (byte 8 8) width)
          (aref data 3) (ldb (byte 8 0) width)
          (aref data 4) (ldb (byte 8 24) height)
          (aref data 5) (ldb (byte 8 16) height)
          (aref data 6) (ldb (byte 8 8) height)
          (aref data 7) (ldb (byte 8 0) height)
          (aref data 8) 8
          (aref data 9) (image-png-color-type image)
          (aref data 10) 0
          (aref data 11) 0
          (aref data 12) 0)
    (write-png-chunk data +png-ihdr-chunk-type+ stream)))

(defun write-png-end-chunk (stream)
  (write-png-chunk #() +png-iend-chunk-type+ stream))

(defgeneric write-png-colormap-chunk (stream image))

(defmethod write-png-colormap-chunk (stream (image image)))

(defmethod write-png-colormap-chunk (stream (image indexed-image))
  (let* ((colormap (image-colormap image))
         (colormap-length (length colormap))
         (data (make-array (* 3 colormap-length)
                           :element-type '(unsigned-byte 8))))
    (loop for i below colormap-length
          as color = (aref colormap i)
          do (setf (aref data (* 3 i)) (color-red color)
                   (aref data (+ (* 3 i) 1)) (color-green color)
                   (aref data (+ (* 3 i) 2)) (color-blue color)))
    (write-png-chunk data +png-plte-chunk-type+ stream)))

(defun write-png-data-chunk (stream image)
  (let* ((width (image-width image))
         (height (image-height image))
         (bytes (make-array (* height (1+ (* width (pixel-size image))))
                            :element-type '(unsigned-byte 8))))
    (loop with i = 0
          for y below height
          do (setf (aref bytes i) 0)
             (incf i)
             (loop for x below width
                   do (setf i (write-png-pixel-bytes bytes image x y i))))
    (let ((compressed-bytes (zlib:compress bytes :fixed)))
      (write-png-chunk compressed-bytes +png-idat-chunk-type+ stream))))

(defgeneric image-png-color-type (image))

(defmethod image-png-color-type ((image rgb-image)) 6)

(defmethod image-png-color-type ((image grayscale-image)) 4)

(defmethod image-png-color-type ((image indexed-image)) 3)

(defgeneric write-png-pixel-bytes (dest image x y index))

(defmethod write-png-pixel-bytes (dest (image rgb-image) x y index)
  (let ((color (image-pixel image x y)))
    (setf (aref dest index) (color-red color)
          (aref dest (+ index 1)) (color-green color)
          (aref dest (+ index 2)) (color-blue color)
          (aref dest (+ index 3)) (color-alpha color)))
  (+ index 4))

(defmethod write-png-pixel-bytes (dest (image grayscale-image) x y index)
  (let ((gray (image-pixel image x y)))
    (setf (aref dest index) (gray-intensity gray)
          (aref dest (1+ index)) (gray-alpha gray)))
  (+ index 2))

(defmethod write-png-pixel-bytes (dest (image indexed-image) x y index)
  (setf (aref dest index) (image-pixel image x y))
  (1+ index))
