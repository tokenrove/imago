;;; IMAGO library
;;; Color operations
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(deftype rgb-pixel () '(unsigned-byte 32))

(deftype grayscale-pixel () '(unsigned-byte 16))

(deftype binary-pixel () 'bit)

(deftype indexed-pixel () '(unsigned-byte 8))

(deftype planar-pixel (&optional (plane-count '*))
  `(unsigned-byte ,plane-count))

(declaim (inline make-gray gray-intensity gray-alpha invert-gray)
         (ftype (sera:-> (grayscale-pixel) (values sera:octet &optional))
                gray-intensity gray-alpha))

(sera:-> make-gray
         (sera:octet &optional sera:octet)
         (values grayscale-pixel &optional))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gray (intensity &optional (alpha #xff))
    (declare (optimize (speed 3)))
    (logior (ash alpha 8) intensity)))

(defun gray-intensity (gray)
  (declare (optimize (speed 3)))
  (ldb (byte 8 0) gray))

(defun gray-alpha (gray)
  (declare (optimize (speed 3)))
  (ldb (byte 8 8) gray))

(sera:-> invert-gray
         (grayscale-pixel)
         (values grayscale-pixel &optional))
(defun invert-gray (gray)
  (logior (logand #x00ff
                  (lognot (logand gray #x00ff)))
          (logand gray #xff00)))

(declaim (inline make-color color-red color-green color-blue color-alpha
                 color-rgb color-argb color-intensity invert-color)
         (ftype (sera:-> (rgb-pixel) (values sera:octet &optional))
                color-red color-green color-blue color-alpha color-intensity))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-color (r g b &optional (alpha #xff))
    (declare (type (unsigned-byte 8) r)
             (type (unsigned-byte 8) g)
             (type (unsigned-byte 8) b)
             (type (unsigned-byte 8) alpha))
    (the rgb-pixel (logior (ash alpha 24) (ash r 16) (ash g 8) b))))

(defun color-red (color)
  (declare (optimize (speed 3)))
  (ldb (byte 8 16) color))

(defun color-green (color)
  (declare (optimize (speed 3)))
  (ldb (byte 8 8) color))

(defun color-blue (color)
  (declare (optimize (speed 3)))
  (ldb (byte 8 0) color))

(defun color-alpha (color)
  (declare (optimize (speed 3)))
  (ldb (byte 8 24) color))

(sera:-> color-rgb
         (rgb-pixel)
         (values sera:octet sera:octet
                 sera:octet &optional))
(defun color-rgb (color)
  (values (color-red color)
          (color-green color)
          (color-blue color)))

(sera:-> color-argb
         (rgb-pixel)
         (values sera:octet sera:octet
                 sera:octet sera:octet
                 &optional))
(defun color-argb (color)
  (values (color-alpha color)
          (color-red color)
          (color-green color)
          (color-blue color)))

(defun color-intensity (color)
  (declare (optimize (speed 3)))
  (multiple-value-bind (r g b)
      (color-rgb color)
    (values
     (floor (+ r g b) 3))))

(sera:-> invert-color
         (rgb-pixel)
         (values rgb-pixel &optional))
(defun invert-color (color)
  (declare (optimize (speed 3)))
  (logior (logand #x00ffffff
                  (lognot
                   (logand color #x00ffffff)))
          (logand color #xff000000)))

(defun closest-colortable-entry (color table)
  (declare (type rgb-pixel color))
  (multiple-value-bind (color-r color-g color-b)
      (color-rgb color)
    (multiple-value-bind (element index score)
        (best-in-array (lambda (c)
                         (multiple-value-bind (r g b)
                             (color-rgb c)
                           (+ (square (- r color-r))
                              (square (- g color-g))
                              (square (- b color-b)))))
                       table
                       :test #'<)
      (declare (ignore element score))
      index)))

(defun make-simple-gray-colormap ()
  (let ((colormap (make-array 256 :element-type 'rgb-pixel)))
    (dotimes (i 256)
      (setf (aref colormap i) (make-color i i i)))
    colormap))

;; Color conversions
;; http://threadlocalmutex.com/?page_id=60
(sera:-> convert-color-to-imago-format ((unsigned-byte 8) (integer 1 8))
         (values (unsigned-byte 8) &optional))
(defun convert-color-to-imago-format (color depth)
  (declare (optimize (speed 3)))
  (ecase depth
    (1 (* color 255))
    (2 (* color 85))
    (3 (ash (1+ (* color 146)) -2))
    (4 (* color 17))
    (5 (ash (+ 23 (* color 527)) -6))
    (6 (ash (+ 33 (* color 259)) -6))
    (7 (ash (+ 64 (* color 257)) -7))
    (8 color)))

;; Constants

(defconstant +white+ (make-color #xff #xff #xff))
(defconstant +black+ (make-color #x00 #x00 #x00))
(defconstant +red+ (make-color #xff #x00 #x00))
(defconstant +green+ (make-color #x00 #xff #x00))
(defconstant +blue+ (make-color #x00 #x00 #xff))
(defconstant +cyan+ (make-color #x00 #xff #xff))
(defconstant +magenta+ (make-color #xff #x00 #xff))
(defconstant +yellow+ (make-color #xff #xff #x00))

(defconstant +default-rgb+  (make-color 0 0 0)
  "Default RGB color for a newly created RGB image")
(defconstant +default-gray+ (make-gray  0)
  "Default grayscale color for a newly created grayscale image")
(defconstant +default-bit+  0
  "Default color for a newly created binary image")
