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

(defun write-integer-as-text (stream number &key (base 10))
  "Writes the text representation of a number to a binary output stream."
  (let ((digits '()))
    (loop with n = number
          while (> n 0)
          do (multiple-value-bind (quotient remainder)
                 (floor n base)
               (push remainder digits)
               (setf n quotient)))
    (loop for digit in digits
          do (write-byte (if (< digit 10)
                             (+ (char-code #\0) digit)
                             (+ (char-code #\A) (- digit 10)))
                         stream))))

(defun read-integer-as-text (stream &key (base 10))
  "Reads the text representation of a number from a binary output stream."
  (loop with number = 0
        as byte = (read-byte stream nil 0)
        as digit = (cond ((null byte) nil)
                         ((<= (char-code #\0) byte (char-code #\9))
                          (- byte (char-code #\0)))
                         ((<= (char-code #\A)
                              byte
                              (+ (char-code #\A) (- base 11)))
                          (+ (- byte (char-code #\A)) 10))
                         (t nil))
        until (null digit)
        do (setf number (+ (* number base) digit))
        finally (return number)))

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

(declaim (inline limit-value))
(defun limit-value (value min max)
  (cond ((< value min) min)
        ((> value max) max)
        (t value)))

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
