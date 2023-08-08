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
(defparameter *image-file-writers* (make-hash-table :test #'equal))

(defun read-image (filename &key (errorp t))
  "Reads an image from a file. If the file format is not recognized,
depending on the value of :ERRORP, either throws an error or returns NIL."
  (declare (type (or pathname string) filename))
  (let ((reader (gethash
                 (string-downcase
                  (pathname-type (pathname filename)))
                 *image-file-readers*)))
    (if (null reader)
        (and errorp (error 'unknown-format :pathname filename))
        (funcall reader filename))))

(defun write-image (image filename &key (errorp t))
  "Writes an image IMAGE to a file. If the file format is not recognized,
depending on the value of :ERRORP, either throws an error or returns NIL."
  (declare (type (or pathname string) filename)
           (type image image))
  (let ((writer (gethash
                 (string-downcase
                  (pathname-type (pathname filename)))
                 *image-file-writers*)))
    (if (null writer)
        (and errorp (error 'unknown-format :pathname filename))
        (funcall writer image filename))))

(defun register-image-io-function (extensions function table)
  (declare (type function function))
  (map nil
       (lambda (extension)
         (declare (type string extension))
         (setf (gethash extension table) function))
       extensions))

(defun register-image-reader (extensions function)
  "Register a reader function for some file extensions. The FUNCTION
must take a FILESPEC as argument, and return an IMAGE."
  (register-image-io-function
   extensions function
   *image-file-readers*))

(defun register-image-writer (extensions function)
  "Register a writer function for some file extensions. The FUNCTION
must take an IMAGE and FILESPEC as arguments. To gain full control of
writing options use specific WRITE-* functions."
  (register-image-io-function
   extensions function
   *image-file-writers*))

(defun register-image-io-functions (extensions &key reader writer)
  "This function is just a shorthand for REGISTER-IMAGE-READER and
REGISTER-IMAGE-WRITER. Whenever READER and WRITER are not NIL, imago
registers that I/O handler for the specified EXTENSIONS."
  (declare (type (or null function) reader writer))
  (when reader
    (register-image-reader extensions reader))
  (when writer
    (register-image-writer extensions writer)))

(defmacro def-reader-from-file (reader-name stream-reader &optional documentation)
  (alex:with-gensyms (stream)
    `(defun ,reader-name (filename)
       ,@(if documentation (list documentation))
       (with-open-file (,stream filename :element-type '(unsigned-byte 8))
         (,stream-reader ,stream)))))

(defmacro def-writer-to-file (writer-name stream-writer keyword-arguments &optional documentation)
  (alex:with-gensyms (stream)
    `(defun ,writer-name (image filename ,@(if keyword-arguments `(&key ,@keyword-arguments)))
       ,@(if documentation (list documentation))
       (with-open-file (,stream filename
                                :direction         :output
                                :if-does-not-exist :create
                                :if-exists         :supersede
                                :element-type      '(unsigned-byte 8))
         (,stream-writer image ,stream
                         ,@(loop for arg in keyword-arguments
                                 for fst = (if (atom arg) arg (car arg)) append
                                 (list (intern (symbol-name fst) (find-package :keyword))
                                       fst)))))))
