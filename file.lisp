;;; IMAGO library
;;; File handling facility
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

(defparameter *image-file-readers* (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :imago-fixed-reader *features*))

(defun read-image (filename &key (errorp t))
  "Reads an image from a file. If the file format is not recognized,
depending on the value of :ERRORP, either throws an error or returns NIL."
  (declare (type (or pathname string) filename))
  (let ((reader (gethash
                 (pathname-type (pathname filename))
                 *image-file-readers*)))
    (if (null reader)
        (and errorp (error "Unknown file format."))
        (funcall reader filename))))

(defun register-image-reader (extensions function)
  "Register a reader function for some file extensions. The FUNCTION
must take a FILESPEC as argument, and return an IMAGE."
  (map nil
       (lambda (extension)
         (setf (gethash extension *image-file-readers*) function))
       extensions))
