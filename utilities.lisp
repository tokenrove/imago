;;; IMAGO library
;;; General utilities
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

;; Types
(deftype image-index     ()
  "Type for an index into image."
  '(unsigned-byte 32))

(deftype image-dimension ()
  "Type for image dimensions."
  '(and image-index (not (eql 0))))

;;; Binary streams

(defun read-msb-integer (stream size)
  (loop with number = 0
        repeat size
        as byte = (read-byte stream)
        do (setf number (logior (ash number 8) byte))
        finally (return number)))

(defun read-lsb-integer (stream size)
  (loop with number = 0
        for i below size
        as byte = (read-byte stream)
        do (incf number (ash byte (* i 8)))
        finally (return number)))

(defun write-msb-integer (number stream size)
  "Writes SIZE bytes of the integer NUMBER to STREAM, in
most-significant bit order."
  (loop for i below size
        for pos = (* (1- size) 8) then (- pos 8)
        do (write-byte (ldb (byte 8 pos) number) stream)))

(defun write-lsb-integer (number stream size)
  "Writes SIZE bytes of the integer NUMBER to STREAM, in
least-significant bit order."
  (loop for i below size
        for pos = 0 then (+ pos 8)
        do (write-byte (ldb (byte 8 pos) number) stream)))

(defun read-byte-array (stream length)
  (let* ((array (make-array length :element-type '(unsigned-byte 8)))
         (read-length (read-sequence array stream)))
    (if (= read-length length)
        array
        (error "EOF while reading sequence"))))

(defun skip-bytes (stream count)
  (loop repeat count
        do (read-byte stream)))

;;; Character streams

(defun skip-line (stream)
  (loop while (char/= (read-char stream) #\newline)))

(defun skip-whitespace-and-comments (stream &optional (comment-start #\#))
  (loop for char = (peek-char nil stream)
        while (member char '(#\space #\tab #\newline #\#) :test #'char=)
        do (read-char stream)
           (when (char= char comment-start)
             (skip-line stream))))

(defun read-number (stream)
  (loop with number = 0
        for charcode = (char-code (peek-char nil stream))
        while (<= (char-code #\0) charcode (char-code #\9))
        do (read-char stream)
           (setf number (+ (* number 10) (- charcode (char-code #\0))))
        finally (return number)))

;;; Miscellaneous

(declaim (inline square))
(defun square (x)
  (* x x))

(defun best-in-array (f array &key (test #'>))
  (let* ((best-index 0)
         (best-element (aref array 0))
         (best-score (funcall f best-element)))
    (loop for index from 1 to (length array)
          as element = (aref array index)
          as score = (funcall f element)
          when (funcall test score best-score)
          do (setf best-index index
                   best-element element
                   best-score score)
          finally (return (values best-element best-index best-score)))))


(sera:-> clamp (fixnum fixnum fixnum) (values fixnum &optional))
(declaim (inline clamp))
(defun clamp (x min max)
  (declare (type fixnum x min max)
           (optimize (speed 3)))
  (max (min x max) min))

(defmacro with-gensyms (vars &body body)
  "Bind variables to gensyms and execute the BODY forms in an
implicit PROGN."
  `(let ,(mapcar (lambda (var)
                   `(,var (gensym)))
                 vars)
    ,@body))

(defmacro read-array-element (array index)
  `(prog1
     (aref ,array ,index)
     (incf ,index)))
