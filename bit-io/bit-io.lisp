;;; IMAGO library
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2022  Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :imago-bit-io)

;; This file contains classes which can be used as binary streams for
;; per-bit access. There are few existing implementations which do a
;; similar job:
;; * Lovesan/trivial-bit-streams — begins reading/writing from LSB,
;;   seems to be abandoned.
;; * psilord/bitio — reader works as needed, but no support for
;;   writing. No gray streams interface. Abandoned.
;;
;; I have to reimplement this stuff once more. This implementation is
;; inspired by Lovesan's trivial-bit-streams.

(deftype bit-counter () '(integer 0 7))
(deftype ub (n) `(unsigned-byte ,n))
(deftype sa-ub (n) `(simple-array (ub ,n) (*)))

(declaim (type alex:positive-fixnum +buffer-length+))
(defconstant +buffer-length+ 4096
  "Length of buffer for BIT-STREAMs.")

(defclass bit-stream (gray:fundamental-binary-stream)
  ((stream :initarg  :stream
           :initform (error "Specify a stream")
           :reader   bit-stream-stream)
   (bit    :initform 0
           :type     bit-counter
           :accessor bit-stream-bit)
   (byte   :initform 0
           :type     alex:non-negative-fixnum
           :accessor bit-stream-byte)
   (end    :initform 0
           :type     alex:non-negative-fixnum
           :accessor bit-stream-end)
   (buffer :initform (make-array +buffer-length+
                                 :element-type '(ub 8))
           :type     (sa-ub 8)
           :accessor bit-stream-buffer))
  (:documentation "Generic bit stream class. Not to be instantiated."))

;; Reading

(defclass bit-input-stream (bit-stream
                            gray:trivial-gray-stream-mixin
                            gray:fundamental-binary-input-stream)
  ())

(declaim (inline ensure-input))
(defun ensure-input (stream)
  (declare (type bit-input-stream stream))
  (when (= (bit-stream-byte stream)
           (bit-stream-end  stream))
    (setf (bit-stream-byte stream) 0
          (bit-stream-end  stream)
          (read-sequence (bit-stream-buffer stream)
                         (bit-stream-stream stream)))))

(declaim (inline inc-bit-counter))
(defun inc-bit-counter (stream)
  (declare (type bit-input-stream stream))
  (with-accessors ((bit  bit-stream-bit)
                   (byte bit-stream-byte))
      stream
    (setf bit (mod (1+ bit) 8))
    (when (zerop bit)
      (incf byte))))

(defmethod gray:stream-read-byte ((stream bit-input-stream))
  (assert (zerop (bit-stream-bit stream)))
  (ensure-input stream)
  (prog1
      (aref (bit-stream-buffer stream)
            (bit-stream-byte   stream))
    (incf (bit-stream-byte stream))))

(sera:-> read-bit (bit-input-stream) (values bit &optional))
(defun read-bit (stream)
  (declare (type bit-input-stream stream))
  (ensure-input stream)
  (prog1
      (ldb (byte 1 (- 7 (bit-stream-bit stream)))
           (aref (bit-stream-buffer stream)
                 (bit-stream-byte   stream)))
    (inc-bit-counter stream)))

(defun skip-to-byte-alignment (stream)
  (declare (type bit-input-stream stream))
  (with-accessors ((bit  bit-stream-bit)
                   (byte bit-stream-byte))
      stream
    (when (not (zerop bit))
      (incf (bit-stream-byte stream))
      (setf (bit-stream-bit  stream) 0)))
  stream)
