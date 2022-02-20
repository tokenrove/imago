;;; IMAGO library
;;; CRC32 checksum calculation
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)


(defparameter +crc32-table+
  (loop with table = (make-array 256 :element-type '(unsigned-byte 32))
	for i below 256
	for c = i
	do (loop for k below 8
                 do (setf c (if (= (logand c 1) 1)
                                (logxor (ash c -1) #xedb88320)
                                (ash c -1))))
	   (setf (aref table i) c)
        finally (return table)))

(defun update-crc32 (crc buffer)
  (setf crc (logxor crc #xffffffff))
  (loop for n below (length buffer)
	for i = (logand #xff (logxor crc (aref buffer n)))
	do (setf crc (logxor (aref +crc32-table+ i) (ash crc -8)))
	finally (return (logxor crc #xffffffff))))

(defun crc32 (buffer)
  (update-crc32 0 buffer))
