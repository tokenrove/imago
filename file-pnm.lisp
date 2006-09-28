;;; IMAGO library
;;; PNM file handling
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defun read-pnm (filespec)
  "Reads data for an image in PNM format from a file, and returns
a newly created image correponding to those data."
  (with-open-file (stream filespec :direction :input)
    (read-pnm-from-stream stream)))

(defun read-number2 (stream)
  (skip-whitespace-and-comments stream)
  (read-number stream))

(defun read-ascii-rgb-pixel (stream)
  (let* ((r (read-number2 stream))
         (g (read-number2 stream))
         (b (read-number2 stream)))
    (make-color r g b)))

(defun read-bin-rgb-pixel (stream)
  (let* ((r (char-code (read-char stream)))
         (g (char-code (read-char stream)))
         (b (char-code (read-char stream))))
    (make-color r g b)))

(defparameter +pnm-image-makers+
  `((2 . ,#'(lambda (width height)
              (make-instance 'grayscale-image :width width :height height)))
    (3 . ,#'(lambda (width height)
              (make-instance 'rgb-image :width width :height height)))
    (5 . ,#'(lambda (width height)
              (make-instance 'grayscale-image :width width :height height)))
    (6 . ,#'(lambda (width height)
              (make-instance 'rgb-image :width width :height height)))))
  
(defparameter +pnm-pixel-readers+
  `((2 . ,#'read-number2)
    (3 . ,#'read-ascii-rgb-pixel)
    (5 . ,#'(lambda (stream) (char-code (read-char stream))))
    (6 . ,#'read-bin-rgb-pixel)))

(defun read-pnm-from-stream (stream)
  (assert (char= (read-char stream) #\P))
  (let* ((magic-number (- (char-code (read-char stream)) #.(char-code #\0)))
         (image-maker (cdr (assoc magic-number +pnm-image-makers+)))
         (pixel-reader (cdr (assoc magic-number +pnm-pixel-readers+))))
    (multiple-value-bind (width height max-value)
        (read-pnm-info stream)
      (declare (ignore max-value))
      (let ((image (funcall image-maker width height)))
        (loop for y below height
              do (loop for x below width
                       do (setf (image-pixel image x y)
                                (funcall pixel-reader stream))))
        image))))

(defun read-pnm-info (stream)
  (let ((width 0)
        (height 0)
        (max-value 0))
    (skip-whitespace-and-comments stream)
    (setf width (read-number stream))
    (skip-whitespace-and-comments stream)
    (setf height (read-number stream))
    (skip-whitespace-and-comments stream)
    (setf max-value (read-number stream))
    (skip-line stream)
    (values width height max-value)))

(register-image-reader '("pnm" "PNM" "ppm" "PPM" "pgm" "PGM") #'read-pnm)


(defun write-pnm (image filespec output-format)
  "Writes the image data to a file in PNM format.
OUTPUT-FORMAT can be either :ASCII or :BINARY."
  (with-open-file (stream filespec :direction :output :if-exists :supersede)
    (write-pnm-to-stream image stream output-format))
  image)

(defgeneric write-pnm-to-stream (image stream output-format))

(defmacro with-write-pnm-loop ((stream x y pixels
                                magic-number max-value)
                               &body body)
  `(let ((height (car (array-dimensions ,pixels)))
         (width (cadr (array-dimensions ,pixels))))
    (format ,stream "P~A~%" ,magic-number)
    (format ,stream "~A ~A~%" width height)
    (when ,max-value
      (format ,stream "~A~%" ,max-value))
    (loop for ,y below height
          do (loop for ,x below width
                   do ,@body))
    nil))

(defmethod write-pnm-to-stream ((image rgb-image) stream
                                (output-format (eql :ascii)))
  (let ((pixels (image-pixels image)))
    (with-write-pnm-loop (stream x y pixels 3 255)
      (let ((rgb (image-pixel image x y)))
        (format stream "~A ~A ~A~%"
                (color-red rgb)
                (color-green rgb)
                (color-blue rgb))))))

(defmethod write-pnm-to-stream ((image rgb-image) stream
                                (output-format (eql :binary)))
  (let ((pixels (image-pixels image)))
    (with-write-pnm-loop (stream x y pixels 6 255)
      (let ((rgb (image-pixel image x y)))
        (write-char (code-char (color-red rgb)) stream)
        (write-char (code-char (color-green rgb)) stream)
        (write-char (code-char (color-blue rgb)) stream)))))

(defmethod write-pnm-to-stream ((image grayscale-image) stream
                                (output-format (eql :ascii)))
  (let ((pixels (image-pixels image)))
    (with-write-pnm-loop (stream x y pixels 2 255)
      (format stream "~A~%" (image-pixel image x y)))))

(defmethod write-pnm-to-stream ((image grayscale-image) stream
                                (output-format (eql :binary)))
  (let ((pixels (image-pixels image)))
    (with-write-pnm-loop (stream x y pixels 5 255)
      (write-char (code-char (image-pixel image x y)) stream))))

(defmethod write-pnm-to-stream ((image indexed-image) stream
                                (output-format (eql :ascii)))
  (let ((pixels (image-pixels image))
	(color-map (image-colormap image)))
    (with-write-pnm-loop (stream x y pixels 3 255)
      (let ((pixel-rgb (aref color-map (image-pixel image x y))))
        (format stream "~A ~A ~A~%"
                (color-red pixel-rgb)
                (color-green pixel-rgb)
                (color-blue pixel-rgb))))))

(defmethod write-pnm-to-stream ((image indexed-image) stream
                                (output-format (eql :binary)))
  (let ((pixels (image-pixels image))
	(color-map (image-colormap image)))
    (with-write-pnm-loop (stream x y pixels 6 255)
      (let ((pixel-rgb (aref color-map (image-pixel image x y))))
        (write-char (code-char (color-red pixel-rgb)) stream)
        (write-char (code-char (color-green pixel-rgb)) stream)
        (write-char (code-char (color-blue pixel-rgb)) stream)))))


