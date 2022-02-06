;;; IMAGO library
;;; PNM file handling
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2022 Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

;; Reading
(defun magic=>image-type+format (magic)
  (case magic
    (1 (values 'binary-image    :ascii))
    (2 (values 'grayscale-image :ascii))
    (3 (values 'rgb-image       :ascii))
    (5 (values 'grayscale-image :binary))
    (6 (values 'rgb-image       :binary))
    (t (error  'decode-error
              :format-control   "Unknown magic number: ~d"
              :format-arguments (list magic)))))

(defgeneric read-pnm-pixel (image format stream))

(defmethod read-pnm-pixel ((image binary-image)
                           (format (eql :ascii))
                           stream)
  (- 1 (read stream)))

(defmethod read-pnm-pixel ((image grayscale-image)
                           (format (eql :ascii))
                           stream)
  (make-gray (read stream)))

(defmethod read-pnm-pixel ((image rgb-image)
                           (format (eql :ascii))
                           stream)
  (let* ((r (read stream))
         (g (read stream))
         (b (read stream)))
    (make-color r g b)))

(defmethod read-pnm-pixel ((image grayscale-image)
                           (format (eql :binary))
                           stream)
  (make-gray (read-byte stream)))

(defmethod read-pnm-pixel ((image rgb-image)
                           (format (eql :binary))
                           stream)
  (let* ((r (read-byte stream))
         (g (read-byte stream))
         (b (read-byte stream)))
    (make-color r g b)))

(defun read-pnm-info (stream max-value-p)
  (flet ((read-number% ()
           (prog1
               (read stream)
             (skip-comments stream))))
    (values
     (read-number%)
     (read-number%)
     (if max-value-p (read-number%) 1))))

(defun read-pnm-from-stream (stream)
  "Read PNM image from an octet stream STREAM."
  (declare (type stream stream))
  (let ((stream (flex:make-flexi-stream stream))
        (*read-eval* nil))
    (when (char/= (read-char stream) #\P)
      (error 'decode-error
             :format-control   "Not a PNM image: ~a"
             :format-arguments (list stream)))
    (multiple-value-bind (image-type format)
        (magic=>image-type+format (read stream))
      (skip-comments stream)
      (multiple-value-bind (width height max-value)
          (read-pnm-info stream (not (eq image-type 'binary-image)))
        (declare (ignore max-value))
        (when (eq format :binary)
          (setf (flex:flexi-stream-element-type stream)
                '(unsigned-byte 8)))
        (let ((image (make-instance image-type
                                    :width  width
                                    :height height)))
          (do-image-pixels (image color x y)
            (setf color (read-pnm-pixel image format stream)))
          image)))))

(defun read-pnm (filespec)
  "Reads data for an image in PNM format from a file, and returns
a newly created image correponding to those data."
  (declare (type (or string pathname) filespec))
  (with-open-file (stream filespec
                          :direction     :input
                          :element-type '(unsigned-byte 8))
    (read-pnm-from-stream stream)))

;; Writing
(defun image-type+format=>magic (image format)
  (let ((type (type-of image)))
    (cond
      ((and (eq type   'binary-image)
            (eq format :ascii))
       1)
      ((and (eq type   'grayscale-image)
            (eq format :ascii))
       2)
      ((and (eq type   'grayscale-image)
            (eq format :binary))
       5)
      ((and (eq type   'rgb-image)
            (eq format :ascii))
       3)
      ((and (eq type   'rgb-image)
            (eq format :binary))
       6)
      (t (error 'encode-error
                :format-control   "Unknown magic number for ~a image ~a"
                :format-arguments (list format image))))))

(defgeneric pnm-output-max-value-p (image)
  (:method ((image binary-image))
    nil)
  (:method ((image image))
    t))

(defgeneric pnm-write-pixel (image format stream pixel))

(defun write-pnm-to-stream (image stream format)
  "Write IMAGE to an octet stream STREAM. FORMAT can be either :BINARY
or :ASCII."
  (declare (type image image)
           (type stream stream)
           (type (member :binary :ascii) format))
  (let ((stream (flex:make-flexi-stream stream)))
    (format stream "P~d~%" (image-type+format=>magic image format))
    (format stream "~d ~d ~@[~d~]~%"
            (image-width  image)
            (image-height image)
            (if (pnm-output-max-value-p image) 255))
    (when (eq format :binary)
      (setf (flex:flexi-stream-element-type stream)
            '(unsigned-byte 8)))
    (do-image-pixels (image color x y)
      (pnm-write-pixel image format stream color)))
  image)
  
  
(defmethod pnm-write-pixel ((image rgb-image)
                            (format (eql :ascii))
                            stream pixel)
  (format stream "~d ~d ~d~%"
          (color-red   pixel)
          (color-green pixel)
          (color-blue  pixel)))

(defmethod pnm-write-pixel ((image grayscale-image)
                            (format (eql :ascii))
                            stream pixel)
  (format stream "~d~%"
          (gray-intensity pixel)))

(defmethod pnm-write-pixel ((image binary-image)
                            (format (eql :ascii))
                            stream pixel)
  (format stream "~d~%"
          (- 1 pixel)))

(defmethod pnm-write-pixel ((image rgb-image)
                            (format (eql :binary))
                            stream pixel)
  (write-byte (color-red   pixel) stream)
  (write-byte (color-green pixel) stream)
  (write-byte (color-blue  pixel) stream))

(defmethod pnm-write-pixel ((image grayscale-image)
                            (format (eql :binary))
                            stream pixel)
  (write-byte (gray-intensity pixel) stream))

(defun write-pnm (image filespec output-format)
  "Writes the image data to a file in PNM format.
OUTPUT-FORMAT can be either :ASCII or :BINARY."
  (declare (type image image)
           (type (or string pathname) filespec)
           (type (member :binary :ascii) output-format))
  (with-open-file (stream filespec
                          :direction         :output
                          :if-does-not-exist :create
                          :if-exists         :supersede
                          :element-type      '(unsigned-byte 8))
    (write-pnm-to-stream image stream output-format))
  image)

(register-image-io-functions '("pnm" "ppm" "pgm" "pbm")
                             :reader #'read-pnm
                             :writer #'(lambda (image filespec)
                                         ;; TODO: need parameter here
                                         (write-pnm image filespec :ascii)))
