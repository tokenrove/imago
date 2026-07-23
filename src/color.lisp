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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sera:-> make-gray (sera:octet &optional sera:octet)
           (values grayscale-pixel &optional))
  (declaim (inline make-gray))
  (defun make-gray (intensity &optional (alpha #xff))
    (logior (ash alpha 8) intensity)))

(sera:-> gray-intensity (grayscale-pixel)
         (values sera:octet &optional))
(declaim (inline gray-intensity))
(defun gray-intensity (gray)
  (ldb (byte 8 0) gray))

(sera:-> gray-alpha (grayscale-pixel)
         (values sera:octet &optional))
(declaim (inline gray-alpha))
(defun gray-alpha (gray)
  (ldb (byte 8 8) gray))

(sera:-> invert-gray (grayscale-pixel)
         (values grayscale-pixel &optional))
(defun invert-gray (gray)
  (declare (optimize (speed 3)))
  (make-gray
   (logand (lognot (gray-intensity gray)) #xff)
   (gray-alpha gray)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sera:-> make-color (sera:octet sera:octet sera:octet &optional sera:octet)
           (values rgb-pixel &optional))
  (declaim (inline make-color))
  (defun make-color (r g b &optional (alpha #xff))
    (logior (ash alpha 24) (ash r 16) (ash g 8) b)))

(sera:-> color-red (rgb-pixel)
         (values sera:octet &optional))
(declaim (inline color-red))
(defun color-red (color)
  (ldb (byte 8 16) color))

(sera:-> color-green (rgb-pixel)
         (values sera:octet &optional))
(declaim (inline color-green))
(defun color-green (color)
  (ldb (byte 8 8) color))

(sera:-> color-blue (rgb-pixel)
         (values sera:octet &optional))
(declaim (inline color-blue))
(defun color-blue (color)
  (ldb (byte 8 0) color))

(sera:-> color-alpha (rgb-pixel)
         (values sera:octet &optional))
(declaim (inline color-alpha))
(defun color-alpha (color)
  (ldb (byte 8 24) color))

(sera:-> color-rgba (rgb-pixel)
         (values sera:octet sera:octet sera:octet sera:octet &optional))
(declaim (inline color-rgba))
(defun color-rgba (color)
  (values (color-red   color)
          (color-green color)
          (color-blue  color)
          (color-alpha color)))

(sera:-> color-intensity (rgb-pixel)
         (values sera:octet &optional))
(defun color-intensity (color)
  (declare (optimize (speed 3)))
  (multiple-value-bind (r g b)
      (color-rgba color)
    (values
     (floor (+ r g b) 3))))

(sera:-> invert-color (rgb-pixel)
         (values rgb-pixel &optional))
(defun invert-color (color)
  (declare (optimize (speed 3)))
  (flet ((invert (c)
           (logand (lognot c) #xff)))
    (declare (inline invert))
    (multiple-value-bind (r g b a)
        (color-rgba color)
      (make-color
       (invert r)
       (invert g)
       (invert b)
       a))))

(defun closest-colortable-entry (color table)
  (declare (type rgb-pixel color))
  (multiple-value-bind (color-r color-g color-b)
      (color-rgba color)
    (multiple-value-bind (element index score)
        (best-in-array (lambda (c)
                         (multiple-value-bind (r g b)
                             (color-rgba c)
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
